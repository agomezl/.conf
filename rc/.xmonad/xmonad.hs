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
  xmproc <- spawnPipe "xmobar /home/agomezlo/.xmobarrc"
  xmonad $ desktopConfig
    {manageHook = myHooks <+> manageDocks <+> manageHook defaultConfig
    , startupHook = setWMName "LG3D"
    , layoutHook = avoidStruts $ onWorkspace "Shell" shellLayout $
                                 baseLayout
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
                                        "lxterminal","scrot -s"])
    , ((mod4Mask, xK_a), goToSelected defaultGSConfig)
    , ((mod4Mask, xK_0), gridselectWorkspace defaultGSConfig W.view)
    , ((mod4Mask .|. shiftMask, xK_F10) , spawn "shutdown -h now")
    , ((mod4Mask , xK_KP_Add)           , spawn "amixer set Master 5%+")
    , ((mod4Mask , xK_KP_Subtract)      , spawn "amixer set Master 5%-")
    , ((mod4Mask , xK_KP_Multiply)      , spawn "amixer set Master toggle")
    , ((mod4Mask .|. shiftMask , xK_o)  , spawn "scrot -s -e 'mv $f /home/alien/Pictures/shots'")
    ]

myWorkspaces :: [String]
myWorkspaces = [ "Web", "Emacs", "Shell"] ++ map show [4 .. 9]


myHooks = composeAll
          [className =? "Google-chrome" --> doShift "Web"
          ,className =? "Emacs"--> doShift "Emacs"
          ,className =? "Lxterminal"--> doShift "Shell"
          ]


baseLayout = tall ||| Mirror tall ||| Full
  where  tall = Tall 1 (3/100) (1/2)


shellLayout = Full ||| grid
  where
    grid = named "Grid" $ spacing 2 $ Grid
