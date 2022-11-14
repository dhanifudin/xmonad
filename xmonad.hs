import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust, isNothing)
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseResize
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Reflect
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowArranger
import           XMonad.Prompt
import qualified XMonad.StackSet              as W
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Replace
import           XMonad.Util.Run              (spawnPipe)

myFont                      = "xft:Fira Code-10:bold"

myNormalBorderColor         = "#333333"
myFocusedBorderColor        = "#0088CC"

myBorderWidth               = 4
myModMask                   = mod4Mask
myTerminal                  = "kitty"
myWorkspaces                = ["Q","W","E","R","T","Y","U","I","O","P"]

myManageHook                = composeAll
  [ className             =? "TelegramDesktop" --> doShift "P"
  , className             =? "Slack" --> doShift "P"
  , className             =? "firefox" <&&> resource =? "Toolkit" --> doFloat
  , className             =? "mpv"  --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)
  ]

myLayoutHook                = mouseResize $ windowArrange $ onWorkspace "Q" termLayout $ onWorkspace "P" messageLayout $ defaultLayout
    where
      defaultLayout       = avoidStruts ( TwoPane (3/100) (1/2) ||| tall ||| Full)
          where
              tall        = Tall nmaster delta ratio
              nmaster     = 1
              ratio       = 2/3
              delta       = 1/40

      termLayout          = avoidStruts ( TwoPane (3/100) (1/2) ||| Grid ||| Full )

      messageLayout       = avoidStruts ( TwoPane (3/100) (1/2) ||| Full )

skipFloating :: (Eq a, Ord a) => W.StackSet i l a s sd -> (W.StackSet i l a s sd -> W.StackSet i l a s sd) -> W.StackSet i l a s sd
skipFloating stacks f
    | isNothing curr = stacks -- short circuit if there is no currently focused window
    | otherwise = skipFloatingR stacks curr f
  where curr = W.peek stacks

skipFloatingR :: (Eq a, Ord a) => W.StackSet i l a s sd -> (Maybe a) -> (W.StackSet i l a s sd -> W.StackSet i l a s sd) -> W.StackSet i l a s sd
skipFloatingR stacks startWindow f
    | isNothing nextWindow = stacks -- next window is nothing return current stack set
    | nextWindow == startWindow = newStacks -- if next window is the starting window then return the new stack set
    | M.notMember (fromJust nextWindow) (W.floating stacks) = newStacks -- if next window is not a floating window return the new stack set
    | otherwise = skipFloatingR newStacks startWindow f -- the next window is a floating window so keep recursing (looking)
  where newStacks = f stacks
        nextWindow = W.peek newStacks

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((0, 0x1008ff12), spawn "amixer set Master toggle")
    , ((0, 0x1008ff11), spawn "amixer set Master 5%-")
    , ((0, 0x1008ff13), spawn "amixer set Master 5%+")
    , ((modMask, xK_space), spawn "rofi -show run")
    , ((modMask, xK_g), goToSelected def)
    , ((modMask, xK_c), spawn "clipmenu")
    , ((modMask, xK_s), spawn "scrot --delay 1 ~/Pictures/screenshots/screen_%Y-%m-%d %H-%M-%S.png")
    , ((modMask .|. shiftMask, xK_s), spawn "scrot --select ~/Pictures/screenshots/window_%Y-%m-%d %H-%M-%S.png")
    , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask, xK_x), kill1)
    , ((modMask, xK_Tab), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_Tab), setLayout $XMonad.layoutHook conf)
    , ((modMask, xK_j), windows (\s -> skipFloating s W.focusDown))
    , ((modMask, xK_k), windows (\s -> skipFloating s W.focusUp))
    , ((modMask, xK_m), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    -- , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_x), io (exitWith ExitSuccess))
    --, ((modMask .|. shiftMask, xK_r), restart "xmonad" True)
    ]
    ++

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_u, xK_i, xK_o, xK_p]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, controlMask)]]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myStartupHook :: X ()
myStartupHook = do
    setDefaultCursor xC_left_ptr >> setWMName "LG3D"
    spawn "~/.xmonad/autostart"

main = xmonad $ docks $ ewmhFullscreen . ewmh $ def
  { terminal              = myTerminal
  , borderWidth           = myBorderWidth
  , focusFollowsMouse     = myFocusFollowsMouse
  , modMask               = myModMask
  , workspaces            = myWorkspaces
  , normalBorderColor     = myNormalBorderColor
  , focusedBorderColor    = myFocusedBorderColor
  , keys                  = myKeys
  , mouseBindings         = myMouseBindings
  , layoutHook            = myLayoutHook
  , manageHook            = manageDocks <+> myManageHook
  , startupHook           = myStartupHook
  }
