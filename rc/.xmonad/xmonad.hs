import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Util.Loggers (logCurrent)
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook)
import XMonad.Actions.PhysicalScreens (viewScreen)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar /home/agomezl/.xmobarrc"
  xmonad $ desktopConfig
    { manageHook = myHooks <+> manageDocks <+> manageHook defaultConfig
    , startupHook = setWMName "LG3D"
    , layoutHook = avoidStruts $ onWorkspace "Shell" shellLayout $
                                 onWorkspace "Web" webLayout
                                 baseLayout
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 100
                        } >> fadeHook
    , terminal    = "st"
    , modMask     = mod4Mask
    , focusFollowsMouse = False
    , borderWidth = 2
    , workspaces = myWorkspaces
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_d), spawn "arandr")
    , ((mod4Mask .|. shiftMask, xK_p), spawnSelected defaultGSConfig
                                       ["ec","google-chrome","emacs",
                                        "thunderbird", "virtualbox",
                                        "qbittorrent","vlc",
                                        "lxterminal","scrot -s","firefox",
                                        "minecraft","ts3","steam"])
    , ((mod4Mask, xK_a), goToSelected defaultGSConfig)
    , ((mod4Mask, xK_0), gridselectWorkspace defaultGSConfig W.view)
    , ((mod4Mask .|. shiftMask, xK_F10) , spawn "shutdown -h now")
    , ((0 , 0x1008FF11), spawn "amixer set Master 5%-")
    , ((0 , 0x1008FF13), spawn "amixer set Master 5%+")
    , ((0 , 0x1008FF12), spawn "amixer set Master toggle")
    , ((mod4Mask, xK_q), viewScreen 0)
    , ((mod4Mask, xK_w), viewScreen 1)
    , ((mod4Mask, xK_e), viewScreen 2)
    , ((mod4Mask, xK_r), spawn "xmonad --recompile && xmonad --restart || xmessage xmonad error ")
    ]

myWorkspaces :: [String]
myWorkspaces = [ "Web", "Emacs", "Shell"] ++ map show [4 .. 9]


myHooks = composeAll
          [className =? "Google-chrome" --> doShift "Web"
          ,className =? "Emacs"--> doShift "Emacs"
          ,className =? "xterm-256color"--> doShift "Shell"
          ,className =? "Wow-64.exe" --> doShift "4"
          ,className =? "Wow-64.exe" --> doFloat
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
    ratio  = 16/20
    delta  = 3/100

fadeHook :: X ()
fadeHook = do
  ws <- logCurrent
  case ws of
   Just "Shell" -> fadeInactiveCurrentWSLogHook 0.9
   _            -> fadeInactiveCurrentWSLogHook 1
