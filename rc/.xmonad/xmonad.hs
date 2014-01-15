import XMonad
import XMonad.Config.Gnome
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Fullscreen
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import System.IO

main = do
  xmproc <- spawnPipe "/home/alien/.cabal/bin/xmobar /home/alien/.xmobarrc"
  xmonad $ desktopConfig
    {manageHook = myHooks <+> manageDocks <+> manageHook defaultConfig
    , startupHook = setWMName "LG3D"
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 100
                        }
    ,terminal    = "lxterminal"
    , modMask     = mod4Mask
    , focusFollowsMouse = False
    , borderWidth = 2
    , workspaces = myWorkspaces
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_d), spawn "arandr")
    , ((mod4Mask , xK_d), spawn "pcmanfm")
    , ((mod4Mask .|. shiftMask, xK_p), spawnSelected defaultGSConfig
                                       ["ec","google-chrome","emacs",
                                        "lxterminal", "virtualbox",
                                        "qbittorrent","vlc"])
    , ((mod4Mask, xK_a), goToSelected defaultGSConfig)
    , ((mod4Mask, xK_0), gridselectWorkspace defaultGSConfig W.view)
    , ((mod4Mask , xK_o), spawn "sh /home/alien/.xmonad/restore.sh")
    , ((mod4Mask .|. shiftMask, xK_m), spawn "sh /home/alien/.xmonad/multi.sh")
    , ((mod4Mask , xK_z), spawn "touch ~/.pomodoro_session")
    , ((mod4Mask .|. shiftMask, xK_F10), spawn "shutdown -h now")
    , ((0 , 0x1008FF11), spawn "amixer set Master 5%-")
    , ((0 , 0x1008FF13), spawn "amixer set Master 5%+")
    , ((0 , 0x1008FF12), spawn "amixer set Master toggle")
    , ((mod4Mask .|. shiftMask, xK_o), spawn "scrot -s -e 'mv $f /home/alien/Pictures/shots'")
    , ((mod4Mask .|. shiftMask, xK_F11), spawn "pm-hibernate")
    ]

myWorkspaces = [ "Web", "Emacs", "Shell" ] ++ map show [4 .. 9]
myHooks = composeAll
          [className =? "Google-chrome" --> doShift "Web"
          ,className =? "Emacs"--> doShift "Emacs"
          ,className =? "Lxterminal"--> doShift "Shell"
          ]
