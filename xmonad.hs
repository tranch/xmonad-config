import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (ToggleStruts(..),avoidStruts,docksEventHook,docks,manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat,isFullscreen)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedWindows
import System.IO

import qualified XMonad.StackSet as W


main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ withUrgencyHook LibNotifyUrgencyHook 
        $ docks defaultConfig
        { terminal              = "urxvt"
        , focusedBorderColor    = "#cfcfcf"
        , normalBorderColor     = "#373b41"
        , borderWidth           = 2
        -- , logHook               = dynamicLogWithPP $ xmobarPP {
        --                           ppOutput = hPutStrLn xmproc
        --                         , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
        --                         , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
        --                         , ppSep = "   "
        -- }
        , manageHook            = composeAll
                                [ isFullscreen --> doFullFloat
                                , appName =? "albert" --> doFloat
                                , manageHook defaultConfig ]
        , layoutHook            = spacing 2 $ avoidStruts $ layoutHook defaultConfig
        , startupHook           = myStartupHook
        , handleEventHook       = handleEventHook defaultConfig <+> docksEventHook
        } `additionalKeys`
        [ ((0, 0x1008ff11), spawn "amixer set Master 5%- unmute")
        , ((0, 0x1008ff12), spawn "amixer set Master toggle")
        , ((0, 0x1008ff13), spawn "amixer set Master 5%+ unmute")
        , ((0, xK_Print), spawn "scrot -q 1 $HOME/Pictures/screenshots/%Y%m%d_%H%M%S.png")
        , ((controlMask, xK_Print), spawn "scrot -u $HOME/Pictures/screenshots/%Y%m%d_%H%M%S.png")
        , ((mod4Mask, xK_space), spawn "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun")
        ]

xmobarTitleColor = "#22CCDD"

myStartupHook = do
    setWMName "LG3D"

xmobarCurrentWorkspaceColor = "#CEFFAC"

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]
