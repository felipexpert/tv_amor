{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- MyModule.hs
module Model.AniAutoTask where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 
import Model.EpisodeComplete
import Model.AudiosInfo(AudioInfo(..), aiDuration, aiFilePath, AudiosInfo(..), AudiosRequest(..), AudioRequest(..), AudioRequestConfig(..))

import Model.EpisodePersona (EPeNumber(..))
import Model.Episode(CGesture(..))
import Model.EpisodePersona(EPeLabel(..))

import Model.GuidoLangUtil (glCall, GLScript(GLAudiosInfo))

import qualified Model.Episode as E
import qualified Model.Episode as E

data AniAutoTask = AniAutoTask
    { aatActions :: [TPeAction]
    , aatTotalDuration :: Int -- Duração total do episódio em milissegundos
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
episodeToTaskDialoguesIO ep = mapM dialoguePeToTaskDialogue (E.eDialoguePeList ep)
    where
        dialoguePeToTaskDialogue :: E.EDialoguePe -> IO TDialoguePe
        dialoguePeToTaskDialogue edp = do
            let peLabel = E.dPe edp
                peNumber :: EPeNumber
                peNumber = E.episodePeNumber ep peLabel

            contentsWithAudio <- processDRichText (E.dContents edp)
            return $ TDialoguePe
                { dPe = peLabel
                , dPeNumber = peNumber
                , dContents = contentsWithAudio
                }
        
        processDRichText :: [E.DRichText] -> IO [DRichText]
        processDRichText texts = do
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

episodeCompleteToAniAutoTaskIO :: EpisodeComplete -> IO AniAutoTask
episodeCompleteToAniAutoTaskIO ec = do
    -- começa pelos audios
    aInfos <- requestAudiosIO audiosRequest -- aqui você cria o AudiosRequest com base
    undefined
    where
        audiosRequest :: AudiosRequest
        audiosRequest = undefined
        episode :: Episode
        episode = ecEpisode ec