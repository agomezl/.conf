import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import System.IO

main :: IO ()
main = do
  xmproc <- spawnPipe "/home/alien/.cabal/bin/xmobar /home/alien/.xmobarrc"
  xmonad $ desktopConfig
    {manageHook = myHooks <+> manageDocks <+> manageHook defaultConfig
    , startupHook = setWMName "LG3D"
    , layoutHook = avoidStruts $ onWorkspace "Shell" shellLayout $
                                 onWorkspace "Mail" Full $
                                 onWorkspace "Web" webLayout
                                 baseLayout
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 100
                        }
    ,terminal    = "urxvt"
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
                                        "thunderbird", "virtualbox",
                                        "qbittorrent","vlc","matlab",
                                        "lxterminal","scrot -s"])
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

myWorkspaces :: [String]
myWorkspaces = [ "Web", "Emacs", "Shell","Mail"] ++ map show [5 .. 9]


myHooks = composeAll
          [className =? "Google-chrome" --> doShift "Web"
          ,className =? "Emacs"--> doShift "Emacs"
          ,className =? "urxvt"--> doShift "Shell"
          ,className =? "Thunderbird" --> doShift "Mail"
          ]


baseLayout = tall ||| Mirror tall ||| Full
  where  tall = Tall 1 (3/100) (1/2)


shellLayout = Full ||| grid
  where
    grid = named "Grid" $ spacing 2 $ Grid

webLayout = Full ||| sideTall
  where
    sideTall = Tall nmaster delta ratio
    nmaster = 1
    ratio  = 4/5
    delta  = 3/100
