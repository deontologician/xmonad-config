--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import Data.Time
import System.Locale
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Tabbed
import XMonad.Layout.Spiral
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Layout.NoBorders
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.SetWMName
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow
import XMonad.Hooks.UrgencyHook
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Ratio ((%))

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "sakura -e fish" -- Run sakura with fish

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
--greeks = ["α","β","δ","ζ","λ","μ","π","Σ","Ω"]
--myWorkspaces = greeks ++ map show [(length greeks) .. (length greeks + 10)]
myWorkspaces = ["Home"]
-- myWorkspaces = map show [1..]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#000099"

randomWallpaper :: (MonadIO m) => m ()
randomWallpaper = spawn "feh --bg-scale `ls ~/Wallpapers/* | sort -R | head -1`"

xF86XK_Battery :: KeySym
xF86XK_Battery = 269025171

gripeFile :: String
gripeFile = "/home/habitue/gripes"

changeLog :: String
changeLog = "/home/habitue/changelog"

-- Get a string version of the current timestamp
getTimeStamp :: MonadIO m => m String
getTimeStamp = do 
  tz <- liftIO getCurrentTimeZone
  time <- liftIO getCurrentTime
  return . formatTime defaultTimeLocale "%m/%-e/%Y %I:%M%p" . utcToLocalTime tz $ time

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawnHere $ XMonad.terminal conf)

    -- launch Xmonad shellprompt
    , ((modm,               xK_p     ), shellPromptHere defaultXPConfig)

    -- close focused window on this workspace (kill if not on any other workspace)
    , ((modm .|. shiftMask, xK_c     ), kill1)
    -- kill focused window on all workspaces
    , ((modm .|. controlMask, xK_c   ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Dynamic Workspace keybindings
    -- Delete current workspace (and move windows to some other workspace)
    , ((modm              , xK_d      ), removeWorkspace)
    -- Select or create a new workspace from an autocomplete list
    , ((modm .|. shiftMask, xK_v      ), selectWorkspace defaultXPConfig)
    -- Move the currently selected window to another workspace
    , ((modm              , xK_m      ), withWorkspace defaultXPConfig 
                                           (windows . W.shift))
    -- Copy the currently selected window to another workspace
    , ((modm .|. controlMask, xK_m      ), withWorkspace defaultXPConfig 
                                             (windows . copy))
    -- rename the current workspace
    , ((modm .|. controlMask, xK_r      ), renameWorkspace defaultXPConfig)
    -- Toggle back to the previous workspace
    , ((modm              , xK_0      ), toggleWS )
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Add a line when I want to remember a song name
    , ((modm  .|. controlMask, xK_n), appendFilePrompt defaultXPConfig "/home/habitue/songs.txt")

    -- Add a line when I have a new system gripe with the current timestamp
    , ((modm, xK_g), do
         ts <- (++ ": ") `fmap` getTimeStamp  
         appendFilePrompt defaultXPConfig{defaultText = ts} gripeFile)

    -- Add a line to the changelog with the current date
    , ((modm, xK_c), do 
         ts <- (++ ": ") `fmap` getTimeStamp  -- get the timestamp
         appendFilePrompt defaultXPConfig{defaultText = ts} changeLog )

    -- Move to next nonempty window
    , ((0 , xF86XK_Forward), moveTo Next NonEmptyWS)
    -- Move to previous nonempty window
    , ((0 , xF86XK_Back), moveTo Prev NonEmptyWS)

    -- Move focused to next window
    , ((shiftMask , xF86XK_Forward), shiftToNext)
    -- Move focused to previous window
    , ((shiftMask , xF86XK_Back), shiftToPrev)

    -- Move focused to next window and move there as well
    , ((controlMask , xF86XK_Forward), shiftToNext >> nextWS)
    -- Move focused to previous window and move there as well
    , ((controlMask , xF86XK_Back), shiftToPrev >> prevWS)

    -- Create a new workspace with a random name and move to it
    , ((modm , xF86XK_Forward), do
         newname <- liftIO $ runProcessWithInput "randomword" [] ""
         addWorkspace $ init newname 
         moveTo Next EmptyWS
      )
    -- Move to previous empty window
    , ((modm , xF86XK_Back), moveTo Prev EmptyWS)

    -- Move focused to next empty window
    , ((modm .|. shiftMask , xF86XK_Forward), shiftTo Next EmptyWS)
    -- Move focused to previous empty window
    , ((modm .|. shiftMask , xF86XK_Back), shiftTo Prev EmptyWS)

    -- Move focused to next empty window and move there as well
    , ((modm .|. controlMask , xF86XK_Forward), shiftTo Next EmptyWS >> moveTo Next EmptyWS >> prevWS)
    -- Move focused to previous empty window and move there as well
    , ((modm .|. controlMask , xF86XK_Back), shiftTo Prev EmptyWS >> moveTo Prev EmptyWS >> nextWS)
    -- Toggle last two workspace
    , ((modm , xK_s), toggleWS)

    -- Lock screen with screensaver
    , ((0 , xF86XK_ScreenSaver), spawn "xscreensaver-command -lock")

    -- Put computer to sleep
    , ((0, xF86XK_Sleep), spawn "suspend")

    -- Hibernate
    , ((0, xF86XK_Battery), spawn "hibernate")

    -- Take a screenshot of a specific window
    , ((controlMask       , xK_Print), spawn "sleep 0.2; scrot -s")

    -- Take a screenshot
    , ((0                 , xK_Print), spawn "scrot")

    -- Change wallpaper randomly
    , ((modm              , xK_y), randomWallpaper)

    -- Launch emacs frame (will fail silently if emacs daemon is not running)
    , ((modm .|. shiftMask, xK_m), spawnHere "emacsclient -nc")

    -- Launch firefox
    , ((modm .|. shiftMask, xK_f), spawnHere "firefox")

    -- Launch Chromium
    , ((modm .|. shiftMask, xK_g), spawnHere "chromium")
     
    -- launch a Thunar
    , ((modm .|. shiftMask, xK_t     ), spawnHere "emelfm2")
    
    -- launch Ario
    , ((modm .|. shiftMask, xK_a     ), spawnHere "ario")

    -- XF86AudioMute
    , ((0 , xF86XK_AudioMute), spawn "amixer -q set Master toggle")
    -- XF86AudioLowerVolume
    , ((0 , xF86XK_AudioLowerVolume), spawn "amixer -q set Master 1- unmute")
    -- XF86AudioRaiseVolume
    , ((0 , xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 1+ unmute")
    -- XF86AudioPlay (toggles play/pause)
    , ((0 , xF86XK_AudioPlay), spawn "playorparty")
    -- XF86AudioNext
    , ((0 , xF86XK_AudioNext), spawn "mpc next")
    -- Random next
    , ((controlMask , xF86XK_AudioNext), spawn "playrand")
    -- XF86AudioPrev
    , ((0 , xF86XK_AudioPrev), spawn "mpc prev")
    -- XF86AudioStop
    , ((0 , xF86XK_AudioStop), spawn "mpc stop")

    -- Standard built-in monitor layout (back from VGA)
    , ((modm .|. controlMask, xK_1), 
       spawn "xrandr --output LVDS1 --mode 1280x800 --pos 0x0 --output VGA1 --auto --mode 1280x720" )
    -- Standard built-in monitor layout (back from DVI)
    {-, ((modm .|. controlMask, xK_1), 
       spawn "xrandr --output LVDS --mode 1280x800 --pos 0x0 --output TMDS-1 --auto --mode 1280x720" ) -}
    -- Big monitor layout (VGA)
    , ((modm .|. controlMask, xK_2),
       spawn "xrandr --output LVDS1 --mode 1280x800 --pos 200x1050 --output VGA1 --mode 1680x1050 --pos 0x0" )
    -- Big monitor layout (DVI)
    {- , ((modm .|. controlMask, xK_2),
       spawn "xrandr --output LVDS --mode 1280x800 --pos 200x1050 --output TMDS-1 --mode 1680x1050 --pos 0x0" ) -}
      

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    {-[((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [((W.greedyView), 0), (W.shift, shiftMask)]]
     -}
    ++
    zip (zip (repeat modm) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0.. ])
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--"Ω"]
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- Flexibly resize floating windows
    , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ noBorders $ layoutHook defaultConfig ||| simpleTabbed 
           ||| spiral (6/7) ||| withIM (1%7) (ClassName "Pidgin") Grid

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageSpawn <+> manageDocks <+> manageHook defaultConfig 
               <+> doF W.swapDown <+> composeOne [isFullscreen -?> doFullFloat]
                                                 --, className =? "stalonetray" --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook procname = do
  dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn procname
                            , ppTitle  = xmobarColor "OrangeRed" "" . shorten 50
                            , ppSep    = "  |> "
                            }
  updatePointer $ Relative 0.5 0.5
------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad 
     $ defaultConfig {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      handleEventHook    = myEventHook,
      logHook            = myLogHook xmproc,
      startupHook        = myStartupHook
    }