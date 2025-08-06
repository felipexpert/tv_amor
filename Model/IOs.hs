{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Model.IOs where

-- requestAudiosIO :: AudiosRequest -> IO AudiosInfo
-- requestAudiosIO = glCall GLAudiosInfo

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
        