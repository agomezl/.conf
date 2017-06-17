{-# LANGUAGE UnicodeSyntax #-}
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Operations (refresh)
import XMonad.Util.Loggers (logCurrent)
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook)
import XMonad.Actions.PhysicalScreens (viewScreen)
import XMonad.Prompt (defaultXPConfig,XPConfig(..))
import XMonad.Prompt.Input ((?+),inputPrompt)

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
                                        "thunderbird", "vncviewer",
                                        "Morning","vlc",
                                        "lxterminal","scrot -s","firefox",
                                        "hipchat","spotify","steam"])
    , ((mod4Mask, xK_a), goToSelected defaultGSConfig)
    , ((mod4Mask, xK_0), gridselectWorkspace defaultGSConfig W.view)
    , ((mod4Mask .|. shiftMask, xK_F10) , spawn "shutdown -h now")
    , ((0 , 0x1008FF11), spawn "amixer set Master 5%-")
    , ((0 , 0x1008FF13), spawn "amixer set Master 5%+")
    , ((0 , 0x1008FF12), spawn "amixer set Master toggle")
    , ((mod4Mask, xK_k), kbdSelected defaultGSConfig)
    , ((mod4Mask, xK_j), trackpadSelected defaultGSConfig)
    , ((mod4Mask, xK_q), viewScreen 0)
    , ((mod4Mask, xK_w), viewScreen 1)
    , ((mod4Mask, xK_e), viewScreen 2)
    , ((mod4Mask, xK_r), spawn "xmonad --recompile && xmonad --restart || xmessage xmonad error ")
    , ((mod4Mask, xK_i), isaSelected defaultGSConfig)
    , ((mod4Mask, xK_d), refresh)
    ]

myWorkspaces :: [String]
myWorkspaces = [ "Web", "Edit", "Shell","Chat","Mail"] ++ map show [6 .. 9]


myHooks = composeAll
          [stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "Web"
          ,className =? "Emacs"--> doShift "Edit"
          ,className =? "st-256color"--> doShift "Shell"
          ,className =? "HipChat" --> doShift "Chat"
          ,className =? "Thunderbird" --> doShift "Mail"
          ,className =? "Evolution" --> doShift "Mail"
          ,className =? "jedit" --> doShift "Edit"
          ]


baseLayout = tall ||| Mirror tall ||| smartBorders Full
  where  tall = Tall 1 (3/100) (1/2)


shellLayout = smartBorders Full ||| grid
  where
    grid = named "Grid" $ spacing 2 $ Grid

webLayout = smartBorders Full ||| sideTall
  where
    sideTall = Tall nmaster delta ratio
    nmaster = 1
    ratio  = 16/20
    delta  = 3/100

fadeHook :: X ()
fadeHook = do
  ws <- logCurrent
  case ws of
   Just "Shell" -> fadeInactiveCurrentWSLogHook 0.7
   _            -> fadeInactiveCurrentWSLogHook 1


isaWithImg ∷ String → String → String → X ()
isaWithImg dir arch img = do spawn $ "echo " ++ command
                             spawn command
    where location = "~/NICTA/" ++ dir ++ "/l4v"
          isa_bin  = location ++ "/isabelle/bin/isabelle"
          command  = "L4V_ARCH=" ++ arch ++ " "
                      ++ isa_bin ++ " jedit -d " ++ location
                      ++ " -d "  ++ location ++ "/test-images"
                      ++ " -l "  ++ img

isaPrompt ∷ String → String → X ()
isaPrompt dir arch = inputPrompt myXPConfig "Lauch isabelle Session?" ?+ (isaWithImg dir arch)

myXPConfig = defaultXPConfig { font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*" }

isaSelected :: GSConfig String -> X ()
isaSelected conf = do selection   ← gridselect conf (zip lst lst)
                      arch_choice ← gridselect conf (zip lstArch lstArch)
                      case (arch_choice, selection) of
                        (Just arch, Just dir) -> isaPrompt dir arch
                        _   -> return ()
    where
      lst =["verification", "github"]
      lstArch = ["ARM","ARM_HYP"]

kbdSelected :: GSConfig String -> X ()
kbdSelected conf = do selection ← gridselect conf (zip lst lst)
                      case selection of
                        Just "US"     -> spawn "setxkbmap -layout us"
                        Just "US-INT" -> spawn "setxkbmap -layout us -variant intl"
                        _             -> return ()
    where
      lst =["US", "US-INT"]

trackpadSelected :: GSConfig String -> X ()
trackpadSelected conf = do selection ← gridselect conf (zip lst lst)
                           case selection of
                             Just "TAP"     -> spawn "xinput set-prop 12 277 1"
                             Just "CLICK"   -> spawn "xinput set-prop 12 277 0"
                             _              -> return ()
    where
      lst =["TAP", "CLICK"]
