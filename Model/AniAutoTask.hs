{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use uncurry" #-}

-- MyModule.hs
module Model.AniAutoTask where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 
import Model.EpisodeComplete
import Model.AudiosInfo(AudioInfo(..), aiDuration, aiFilePath, AudiosInfo(..), AudiosRequest(..), AudioRequest(..), AudioRequestConfig(..), requestAudiosIO)

import Model.EpisodePersona (EPeNumber(..))
import qualified Model.EpisodePersona as EP
import Model.Episode(CGesture(..), Episode (Episode))
import Model.EpisodePersona(EPeLabel(..))

import Model.GuidoLangUtil (glCall, GLScript(GLAudiosInfo))

import qualified Model.Episode as E
import qualified Model.EpisodeSetup as ES
import Model.EpisodeComplete (EpisodeComplete(ecEpisode)) 

import qualified Model.Config as C
import qualified System.Directory as SD 

import System.FilePath ((</>))

import Control.Monad (forM_)
import qualified Model.TextUtil as TU


data AniAutoTask = AniAutoTask
    { aatActions :: [TPeAction]
    , aatTotalDuration :: Int -- Duração total do episódio em milissegundos
    -- Precisa por a imagem no tipo AniAutoTask (por que varia a extensão)
    , aatBackgroundImage :: FilePath
    } 
    deriving (Show, Eq, Generic)

data TPeAction = TPeAction
    { tpaNumber :: EPeNumber
    , tpaAction :: AAction
    }
    deriving (Show, Eq, Generic)

data AAction
    = ASpeech
        { asAudioWav :: FilePath -- Caminho do arquivo de áudio
        , asStartTime :: Int -- Tempo de início da fala
        }
    | AGesture
        { agGesture :: CGesture 
        , agStartTime :: Int -- Tempo de início do gesto 
        }
    deriving (Show, Eq, Generic)

-- você pode criar gestos compostos no CA4 e vincular aqui!


-- ***** START - tipo intermediário entre EDialoguePe e AudioInfo e duration - START *****
-- utilizado para criar os TPeAction do AniAutoTask
-- `TDialoguePe` (Task DialoguePe) é o `EDialoguePe` (Episode DialogPe) com informações de áudio
data TDialoguePe = TDialoguePe
    { dPe :: EPeLabel
    , dPeNumber :: EPeNumber
    , dContents :: [DRichText]
    } deriving (Show, Eq)

-- Um trecho de texto com comandos embutidos
data DRichText
    = RPlainText RSpeech
    | RCommand RCCommand
    deriving (Show, Eq)

dRichTextDuration :: DRichText -> Int
dRichTextDuration (RPlainText rs) = aiDuration (rsAudioInfo rs)
dRichTextDuration (RCommand _) = 0 -- Comandos não têm duração de áudio
dRichTextDuration (RCommand (CPause duration)) = duration

data RSpeech = RSpeech
    { rsText :: Text -- Texto a ser falado
    , rsAudioInfo :: AudioInfo -- Informações do áudio associado
    }
    deriving (Show, Eq)

-- Representa os comandos embutidos no texto
data RCCommand 
    -- ERTCGesture tem quem está fazendo o gesto, porque durante a fala de um, o outro pode fazer um gesto
    = CGesture
        { cGesture :: CGesture
        , cPe :: EPeLabel
        , cPeNumber :: EPeNumber
        }
    | CPause Int
    deriving (Show, Eq)
-- ***** END   - tipo intermediário entre EDialoguePe e AudioInfo e duration - END   *****

-- aqui concatenamos os trechos de fala de cada personagem, e somamos o tempo total
dialoguesToActions :: [TDialoguePe] -> ([TPeAction], Int)
dialoguesToActions dialogues = (actions, totalTime)
    where
        actionsAndTimes :: [([TPeAction], Int)]
        actionsAndTimes = map dialogueToActions dialogues
        actions :: [TPeAction]
        actions = concatMap fst actionsAndTimes
        totalTime :: Int
        totalTime = sum $ map snd actionsAndTimes


-- cada trecho em que um personagem fala, gera um conjunto de ações, e o total de tempo
dialogueToActions :: TDialoguePe -> ([TPeAction], Int)
dialogueToActions dialogue = generateActions' (dContents dialogue)
    where
        peNumber :: EPeNumber
        peNumber = dPeNumber dialogue
        generateActions :: [DRichText] -> ([Maybe TPeAction], Int)
        generateActions ts = go ts 0 []
            where
                go [] total acc = (reverse acc, total)
                go (t:ts) currentTime acc = go ts nextTime (actionOpt : acc)
                    where
                        actionOpt = case t of
                            RPlainText speech -> Just $ speechToAction speech
                            -- Atenção! peNumberG é o número do personagem que faz o gesto, pode ser diferente do peNumber
                            RCommand (CGesture gesture _ peNumberG) -> Just $ gestureToAction gesture peNumberG
                            RCommand (CPause duration) -> Nothing
                        nextTime = currentTime + dRichTextDuration t
                        speechToAction :: RSpeech -> TPeAction
                        speechToAction speech = TPeAction peNumber (ASpeech (aiFilePath (rsAudioInfo speech)) currentTime)
                        gestureToAction :: CGesture -> EPeNumber -> TPeAction
                        gestureToAction gesture peNumberG = TPeAction peNumber (AGesture gesture currentTime)
        generateActions' :: [DRichText] -> ([TPeAction], Int)
        generateActions' ts = (actions, totalTime)
            where 
                (actionOpts, totalTime) = generateActions ts
                actions = [a | Just a <- actionOpts] -- filtra as ações válidas

-- funções de IO


-- depois coloca `processAudiosInfoIO` no lugar certo
-- se quiser por uma validação se o texto voltou com os indexes correspondentes corretos, faz
-- lançar uma mensagem de erro se não estiver correto, tem que ter as mesmas posições correspondentes
processAudiosInfoIO :: AudiosRequest -> IO AudiosInfo
processAudiosInfoIO audiosRequest = do
    audiosInfos <- glCall GLAudiosInfo audiosRequest 
    if checkValidity audiosInfos
        then return audiosInfos
        else error "Erro: A resposta do GuidoLang não corresponde ao pedido de áudios"
    where 
        checkValidity :: AudiosInfo -> Bool
        checkValidity (AudiosInfo infos) = sameLength && sameSequence
            where 
                sameLength = length infos == length texts
                sameSequence = and $ zipWith (\(AudioInfo _ _ t1) t2 -> t1 == t2) infos texts
                texts :: [Text]
                texts = [ arText ar | ar <- ars ]
                (AudiosRequest ars) = audiosRequest

episodeToTaskDialoguesIO :: E.Episode -> IO [TDialoguePe]
episodeToTaskDialoguesIO ep = do 
    let -- edp = fmap dialoguePeToTaskDialogue (E.eDialoguePeList ep)
        listOfEdp = fmap E.dContents (E.eDialoguePeList ep)
        allTexts = concat listOfEdp
        -- texts = E.dContents dialoguePeToTaskDialogue edp
    allContentsWithAudio <- processDRichTextIO allTexts
    let allTexts' = zip allTexts allContentsWithAudio
        res = fmap (dialoguePeToTaskDialogue allTexts') (E.eDialoguePeList ep)
    return res
    where
        dialoguePeToTaskDialogue :: [(E.DRichText, DRichText)] -> E.EDialoguePe -> TDialoguePe
        dialoguePeToTaskDialogue allContentsWithAudio edp = res
            where
                peLabel = E.dPe edp
                peNumber :: EPeNumber
                peNumber = E.episodePeNumber ep peLabel
                thisDialogueTexts = E.dContents edp
                contentsWithAudioTuple = filter fromThisDialogue allContentsWithAudio
                    where 
                        fromThisDialogue :: (E.DRichText, DRichText) -> Bool
                        fromThisDialogue (t, tAudio) = t `elem` thisDialogueTexts
                contentsWithAudio = fmap snd contentsWithAudioTuple
                -- contentsWithAudio <- processDRichTextIO (E.dContents edp)
                res = TDialoguePe
                    { dPe = peLabel
                    , dPeNumber = peNumber
                    , dContents = contentsWithAudio
                    }
        
        processDRichTextIO :: [E.DRichText] -> IO [DRichText]
        processDRichTextIO texts = do
            audiosInfo <- processAudiosInfoIO audiosRequest
            let richTexts = richTextsWithAudioAndTimeInfo texts audiosInfo
            return richTexts
            where 
                audiosRequest :: AudiosRequest
                audiosRequest = richTextsToAudiosRequests texts

                richTextsToAudiosRequests :: [E.DRichText] -> AudiosRequest
                richTextsToAudiosRequests richTexts = AudiosRequest $ map textToAudioRequest (richTextsToTexts richTexts)
                    where
                        richTextsToTexts :: [E.DRichText] -> [Text]
                        richTextsToTexts richTexts = [ text | (E.RPlainText text) <- richTexts ]

                        textToAudioRequest :: Text -> AudioRequest
                        textToAudioRequest text = AudioRequest
                            { arText = text
                            , arConfig = AudioRequestConfig { arcVoice = "pt-BR-AntonioNeural" } -- aqui você pode definir a voz padrão ou outra lógica
                            }
                
                -- converte os rich texts do episodio para o formato rich texts com as informações de audios,
                -- que será utilizado para criar o AniAutoTask
                richTextsWithAudioAndTimeInfo :: [E.DRichText] -> AudiosInfo -> [DRichText]
                richTextsWithAudioAndTimeInfo richTexts audiosInfo = map convertRichText richTexts
                    where
                        convertRichText :: E.DRichText -> DRichText
                        convertRichText (E.RPlainText text) = RPlainText $ RSpeech text (findAudioInfo text audiosInfo)
                        convertRichText (E.RCommand command) = RCommand $ convertCommand command

                        findAudioInfo :: Text -> AudiosInfo -> AudioInfo
                        findAudioInfo text (AudiosInfo infos) = head [info | info@(AudioInfo _ _ t) <- infos, t == text]

                        convertCommand :: E.RCCommand -> RCCommand
                        convertCommand (E.CGesture gesture peLabel) =
                            let peNumber = E.episodePeNumber ep peLabel
                            in CGesture gesture peLabel peNumber
                        convertCommand (E.CPause duration) = CPause duration

{-

TDialoguePe ???

dialoguesToActions ??? 

?? dialogueToActions :: TDialoguePe -> ([TPeAction], Int)

buildAniAutoTask :: EpisodeComplete -> AniAutoTask
buildAniAutoTask episodeComplete = AniAutoTask
    { aatActions = actions
    , aatTotalDuration = undefined
    , aatBackgroundImage = bg
    } 
    where
        (actions, totalDuration) = dialogueToActions
        episode = ecEpisode episodeComplete
        setup = ecEpisodeSetup episodeComplete
        bg :: FilePath
        bg = getBG setup
            where
                getBG = ES.bImagePath . ES.sBackgroundImage

-}


episodeCompleteToAniAutoTaskIO :: EpisodeComplete -> IO AniAutoTask
episodeCompleteToAniAutoTaskIO episodeComplete = do
    dialogues <- episodeToTaskDialoguesIO episode
    let (actions, totalDuration) = dialoguesToActions dialogues
    return AniAutoTask
        { aatActions = actions
        , aatTotalDuration = totalDuration
        , aatBackgroundImage = bg
        } 
    where
        episode :: Episode
        episode = ecEpisode episodeComplete
        setup :: ES.EpisodeSetup
        setup = ecEpisodeSetup episodeComplete
        bg :: FilePath
        bg = getBG setup
            where
                getBG = ES.bImagePath . ES.sBackgroundImage

episodeCompleteToAniAutoTaskIO_ :: EpisodeComplete -> IO AniAutoTask
episodeCompleteToAniAutoTaskIO_ ec = do
    -- começa pelos audios
    aInfos <- requestAudiosIO audiosRequest -- aqui você cria o AudiosRequest com base
    undefined
    where
        audiosRequest :: AudiosRequest
        audiosRequest = undefined
        episode :: Episode
        episode = ecEpisode ec


-- função para limpar o diretório de trabalho, para poder fazer o trabalho AniAutoTask
-- encontra a configuração do path do diretório em `config.json`
prepareWorkingDirIO :: C.Config -> ES.EpisodeSetup -> IO ()
prepareWorkingDirIO config episodeSetup = do
    cleanWorkingDirIO
    saveBackgroundImageIO
    saveSpritesIO
    return ()
    where 
        cleanWorkingDirIO :: IO ()
        cleanWorkingDirIO = do
            exists <- SD.doesDirectoryExist workingDir
            if exists
                then SD.removePathForcibly workingDir
                else return ()
            SD.createDirectoryIfMissing True workingDir
            where
                workingDir = C.workingDir config 
        saveBackgroundImageIO :: IO ()
        saveBackgroundImageIO = do
            -- Copiar a imagem `bgFullPath` para `workingDirBgFullPath`
            SD.copyFile bgFullPath workingDirBgFullPath
            return ()
            where 
                workingDirBgFullPath :: FilePath
                workingDirBgFullPath = (C.workingDir config) </> bgImage
                bgFullPath :: FilePath
                bgFullPath = bgBaseDir </> bgImage
                bgBaseDir :: FilePath
                bgBaseDir = C.backgroundDir config
                bgImage :: FilePath
                bgImage = getBG episodeSetup
                getBG :: ES.EpisodeSetup -> FilePath
                getBG = ES.bImagePath . ES.sBackgroundImage
        saveSpritesIO :: IO ()
        saveSpritesIO = do 
            forM_ paths $ \(originDirFile,workingDirFile) ->
                SD.copyFile originDirFile workingDirFile
            where
                spritesList ::[ES.SSprite]
                spritesList = ES.sSprites episodeSetup
                -- o diretório de origin e onde será copiado (originFile,copyFile)
                paths :: [(FilePath, FilePath)]
                paths = fmap mapper spritesList
                    where
                        mapper :: ES.SSprite -> (FilePath, FilePath)
                        mapper sprite = (originDirFile, workingDirFile)
                            where
                                originDirFile = originBaseDir </> originFile
                                    where
                                        originBaseDir :: FilePath
                                        originBaseDir = C.spritesDir config
                                workingDirFile = (C.workingDir config) </> newFile
                                    where
                                        spriteNumber :: EP.EPeNumber
                                        spriteNumber = ES.sNumber sprite
                                        spriteNumber' :: Int
                                        spriteNumber' = EP.ePeNumberToInt spriteNumber
                                        newFile = TU.changeFileNameKeepingExt originFile spriteNumber''
                                            where
                                                spriteNumber'' :: FilePath
                                                spriteNumber'' = show spriteNumber'
                                originFile = ES.sPsdPath sprite