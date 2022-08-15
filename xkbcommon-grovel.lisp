(in-package #:xkb)
(pkg-config-cflags "xkbcommon")
(include "xkbcommon/xkbcommon.h")

(constantenum modifier-names
	      ((:shift "XKB_MOD_NAME_SHIFT"))
	      ((:caps "XKB_MOD_NAME_CAPS"))
	      ((:ctrl "XKB_MOD_NAME_CTRL"))
	      ((:alt "XKB_MOD_NAME_ALT"))
	      ((:num "XKB_MOD_NAME_NUM"))
	      ((:logo "XKB_MOD_NAME_LOGO")))

(constantenum led-names
	      ((:caps "XKB_LED_NAME_CAPS"))
	      ((:num "XKB_LED_NAME_NUM"))
	      ((:scroll "XKB_LED_NAME_SCROLL")))


(ctype keycode "xkb_keycode_t")

(ctype keysym "xkb_keysym_t")

(ctype layout-index "xkb_layout_index_t")

(ctype layout-mask "xkb_layout_mask_t")

(ctype level-index "xkb_level_index_t")

(ctype mod-index "xkb_mod_index_t")

(ctype mod-mask "xkb_mod_mask_t")

(ctype led-index "xkb_led_index_t")

(ctype led-index-mask "xkb_led_mask_t")

(bitfield invalid-masks
	  ((:keycode-invalid "XKB_KEYCODE_INVALID"))
	  ((:layout-invalid "XKB_LAYOUT_INVALID"))
	  ((:level-invalid "XKB_LEVEL_INVALID"))
	  ((:mod-invalid "XKB_MOD_INVALID"))
	  ((:led-invalid "XKB_LED_INVALID")))

(constant (+keycode-max+ "XKB_KEYCODE_MAX"))

(cstruct rule-names "struct xkb_rule_names"
	 (:rules "rules" :type :string)
	 (:model "model" :type :string)
	 (:layout "layout" :type :string)
	 (:variant "variant" :type :string)
	 (:options "options" :type :string))

(cenum keysym-flags
       ((:no-flags "XKB_KEYSYM_NO_FLAGS"))
       ((:case-insensitive "XKB_KEYSYM_CASE_INSENSITIVE")))

(cenum context-flags
       ((:no-flags "XKB_CONTEXT_NO_FLAGS"))
       ((:no-default-includes "XKB_CONTEXT_NO_DEFAULT_INCLUDES")
	:documentation "Create this context with an empty include path.")
       ((:no-evironment-names "XKB_CONTEXT_NO_ENVIRONMENT_NAMES")
	:documentation "Don't take RMLVO names from the environment."))

(cenum compile-flags
       ((:no-flags "XKB_KEYMAP_COMPILE_NO_FLAGS")))

(cenum state-component
	  ((:mods-depressed "XKB_STATE_MODS_DEPRESSED")
	   :documentation "Depressed modifiers, i.e. a key is physically holding them.")
	  ((:mods-latched "XKB_STATE_MODS_LATCHED")
	   :documentation "Latched modifiers, i.e. will be unset after the next non-modifier key press.")
	  ((:mods-locked "XKB_STATE_MODS_LOCKED")
	   :documentation "Locked modifiers, i.e. will be unset after the key provoking the lock has been pressed again.")
	  ((:mods-effective "XKB_STATE_MODS_EFFECTIVE")
	   :documentation "Effective modifiers, i.e. currently active and affect key
processing (derived from the other state components).
Use this unless you explictly care how the state came about.")
	  ((:layout-depressed "XKB_STATE_LAYOUT_DEPRESSED")
	   :documentation "Depressed layout, i.e. a key is physically holding it.")
	  ((:layout-latched "XKB_STATE_LAYOUT_LATCHED")
	   :documentation "Latched layout, i.e. will be unset after the next non-modifier key press.")
	  ((:layout-locked "XKB_STATE_LAYOUT_LOCKED")
	   :documentation "Locked layout, i.e. will be unset after the key provoking the lock has been pressed again.")
	  ((:layout-effective "XKB_STATE_LAYOUT_EFFECTIVE")
	   :documentation "Effective layout, i.e. currently active and affects key processing  (derived from the other state components). Use this unless you explictly care how the state came about.")
	  ((:state-leds "XKB_STATE_LEDS")
	   :documentation "LEDs (derived from the other state components)"))

(cenum state-match
	  ((:match-any "XKB_STATE_MATCH_ANY") :documentation "Returns true if any of the modifiers are active.")
	  ((:match-all "XKB_STATE_MATCH_ALL") :documentation "Returns true if all of the modifiers are active.")
	  ((:match-non-exclusive "XKB_STATE_MATCH_NON_EXCLUSIVE") :documentation "Makes matching non-exclusive, i.e. will not return false if a modifier not specified in the arguments is active."))

(cenum consumed-mode
       ((:mode-xkb "XKB_CONSUMED_MODE_XKB"))
       ((:mode-gtk "XKB_CONSUMED_MODE_GTK")))
