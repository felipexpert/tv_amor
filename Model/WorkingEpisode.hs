{-# LANGUAGE OverloadedStrings #-}

module Model.WorkingEpisode where

import System.Process
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Control.Monad (void)
import System.IO (hClose)

import qualified Data.Text.IO as TIO
import qualified Model.TextUtil as TU

import Model.AudiosInfo
import Model.GuidoLangUtil

import Model.Episode
import Model.EpisodeSetup
import Model.EpisodePersona

import qualified Model.EpisodeComplete as EC
import qualified Model.EpisodeSetup as ES

import qualified Model.AniAutoTask as AAT 

import qualified Model.Config as C

buildEpisodeIO :: IO ()
buildEpisodeIO = do
    config <- C.loadConfigIO
    episodeSetup <- loadEpisodeSetupIO episodeSetupLoader
    let myEpisode = EC.EpisodeComplete episode' episodeSetup
    AAT.prepareWorkingDirIO config episodeSetup
    task <- AAT.episodeCompleteToAniAutoTaskIO myEpisode config
    return ()
    where 
      episode' = addPauseIfNeeded episode

episodeSetupLoader :: EpisodeSetupLoader
episodeSetupLoader = EpisodeSetupLoader
  { eslSprites = 
        [ SSprite (EPeLabel "pe_representante_drogaria_total") "drogaria-total_representante.psd" EPeNum1
        , SSprite (EPeLabel "pe_melhores_ofertas") "melhores-ofertas.psd" EPeNum2
        ]
  , eslBackgroundImageJSON = "drogaria-total-itapira.json"
  , eslCustomExtraPrefsJSONOpt = Nothing
  }

episode :: Episode
episode = Episode
    { ePes = [EPeLabel "pe_cliente", EPeLabel "pe_melhores_ofertas"]
    , eDialoguePeList =
        [ -- Narração 1
          EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Olá. Eu sou o Melhores Ofertas, e hoje estou com você da Allianza Consultoria de Itapira."
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Se você é síndico ou morador, já pensou como reduzir custos do seu condomínio sem perder qualidade?"
                , RCommand (CGesture GThinkShort (EPeLabel "pe_cliente"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Boa pergunta. Muita gente acha que cortar custos é só reduzir serviços, mas não é bem assim."
                , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Aqui na Allianza, a gente trabalha com planejamento estratégico e organização financeira para evitar desperdícios."
                , RCommand (CGesture GStandLong (EPeLabel "pe_cliente"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Além disso, usamos tecnologia para automatizar processos e trazer mais controle no dia a dia."
                , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Ou seja, menos desperdício, mais eficiência e transparência para todos no condomínio."
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Exatamente. O resultado é mais economia e mais tranquilidade para síndicos e moradores."
                , RCommand (CGesture GStandShort (EPeLabel "pe_cliente"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Curta e compartilhe este vídeo para ajudar mais pessoas a encontrar soluções como essa."
                , RCommand (CGesture GStandLong (EPeLabel "pe_melhores_ofertas"))
                ]
            }

        , -- Narração 2
          EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Olá. Eu sou o Melhores Ofertas, e hoje estou com você da Allianza Consultoria de Itapira."
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Você sabia que muitos condomínios pagam mais caro simplesmente por falta de organização?"
                , RCommand (CGesture GWorryShort (EPeLabel "pe_cliente"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Sério. E o pior é que isso acontece sem que o síndico perceba no dia a dia."
                , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Por isso usamos planejamento estratégico para identificar onde estão os gargalos e desperdícios."
                , RCommand (CGesture GThinkLong (EPeLabel "pe_cliente"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "Depois, aplicamos tecnologia e otimização de processos para melhorar toda a gestão."
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Isso significa mais controle financeiro e decisões mais inteligentes no condomínio."
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_cliente"
            , dContents =
                [ RPlainText "E no final, quem ganha são os moradores, com mais transparência e menos custos."
                , RCommand (CGesture GStandLong (EPeLabel "pe_cliente"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Comente elogiando a Allianza Consultoria e ajude mais pessoas a conhecer esse trabalho."
                , RCommand (CGesture GStandLong (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        ]
    }
{-
episode :: Episode
episode = Episode
    { ePes = [EPeLabel "pe_representante_drogaria_total", EPeLabel "pe_melhores_ofertas"]
    , eDialoguePeList =
        [ 
          -- Abertura e Apresentação
          EDialoguePe
            { dPe = EPeLabel "pe_representante_drogaria_total"
            , dContents =
                [ RPlainText "Olá pessoal! Aqui é o representante da Drogaria Total de Itapira."
                , RCommand (CGesture GStandShort (EPeLabel "pe_representante_drogaria_total"))
                , RPlainText "Estamos bem no coração da cidade, na Rua Francisco Glicério, 310."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Oi! Eu sou o Melhores Ofertas, e hoje vim mostrar por que a Drogaria Total é a drogaria que cuida de você em Itapira!"
                , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }

          -- Parte principal: destaque da Drogaria
        , EDialoguePe
            { dPe = EPeLabel "pe_representante_drogaria_total"
            , dContents =
                [ RPlainText "Aqui você encontra medicamentos de marca e genéricos, perfumaria, cosméticos e muito mais."
                , RCommand (CGesture GStandShort (EPeLabel "pe_representante_drogaria_total"))
                , RPlainText "E o melhor: sempre com atendimento próximo e humanizado."
                , RCommand (CGesture GThinkLong (EPeLabel "pe_melhores_ofertas"))
                ]
            }

        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Isso mesmo! A Drogaria Total oferece descontos de até oitenta por cento em produtos selecionados e um programa de fidelidade com prêmios incríveis."
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }

          -- Encerramento e reforço do tema
        , EDialoguePe
            { dPe = EPeLabel "pe_representante_drogaria_total"
            , dContents =
                [ RPlainText "Temos também convênios com empresas, cartão próprio e entrega em domicílio."
                , RCommand (CGesture GThinkShort (EPeLabel "pe_representante_drogaria_total"))
                , RPlainText "Tudo pra facilitar a sua vida e cuidar da sua saúde com carinho."
                ]
            }

        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Então já sabe, né? Drogaria Total Itapira, a drogaria que cuida de você!"
                , RCommand (CGesture GStandLong (EPeLabel "pe_melhores_ofertas"))
                ]
            }

          -- Parte 2: CTA (duas variações)
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Mostre o seu apoio! Comente elogiando a Drogaria Total e curta o vídeo pra ajudar a alcançar mais pessoas!"
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }

        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Curta e compartilhe este vídeo pra que mais pessoas conheçam o cuidado da Drogaria Total Itapira!"
                , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        ]
    }
-}