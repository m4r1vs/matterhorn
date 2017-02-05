module Events.MessageSelect where

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

messagesPerPageOperation :: Int
messagesPerPageOperation = 10

onEventMessageSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMessageSelect st (Vty.EvKey Vty.KEsc []) =
    continue $ st & csMode .~ Main
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) =
    continue $ st & csMode .~ Main
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'k') []) =
    messageSelectUp st >>= continue
onEventMessageSelect st (Vty.EvKey Vty.KUp _) =
    messageSelectUp st >>= continue
onEventMessageSelect st (Vty.EvKey Vty.KPageUp _) =
    messageSelectUpBy messagesPerPageOperation st >>= continue
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'j') []) =
    messageSelectDown st >>= continue
onEventMessageSelect st (Vty.EvKey Vty.KDown _) =
    messageSelectDown st >>= continue
onEventMessageSelect st (Vty.EvKey Vty.KPageDown _) =
    messageSelectDownBy messagesPerPageOperation st >>= continue
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'o') []) =
    openSelectedMessageURLs st >>= continue
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'r') []) =
    beginReplyCompose st >>= continue
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'e') []) =
    beginUpdateMessage st >>= continue
onEventMessageSelect st (Vty.EvKey (Vty.KChar 'd') []) =
    beginConfirmDeleteSelectedMessage st >>= continue
onEventMessageSelect st _ =
    continue st

onEventMessageSelectDeleteConfirm :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMessageSelectDeleteConfirm st (Vty.EvKey (Vty.KChar 'y') []) =
    deleteSelectedMessage st >>= continue
onEventMessageSelectDeleteConfirm st _ =
    continue $ st & csMode .~ Main