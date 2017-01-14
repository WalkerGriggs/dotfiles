// ------------------------------------------------------------
// file:        ~/keymap_qwerty.c
// author:      WalkerGriggs     www.walkergriggs.com
// date:        01_14_17
//
// Modified layout from technomancy's tmk_keyboard fork.
// Found at: github.com/technomancy/tmk_keyboard/tree/atreus/keyboard/atreus
// Originally written by: technomancy (Phil Hagelberg)
// ------------------------------------------------------------


#include "keymap_common.h"

/* The default Atreus layout. First layer is normal keys, second
   (momentary fn layer) is numbers, most punctuation, and
   arrows. Third (modal, persistent) layer is function keys and other
   rarely-used keys. */

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  /* Alpha- (Layer 0)
   * ,------------------------------------------------------------------------.
   * | Q   | W   | E   | R   | T   |     ||     |   Y |   U |   I |   O |   P |
   * |------------------------------------------------------------------------|
   * | A   | S   | D   | F   | G   |     ||     |   H |   J |   K |   L |   ; |
   * |------------------------------------------------------------------------|
   * | Z   | X   | C   | V   | B   |     ||     |   N |   M |   , |   . |   / |
   * |------------------------------------------------------------------------|
   * | esc | tab | sup |shift|bkspc|lctrl|| ltal|paceE|  FN0|   - |   " |enter|
   * '------------------------------------------------------------------------'
   */
  KEYMAP(KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P,    \
         KC_A, KC_S, KC_D, KC_F, KC_G, KC_H, KC_J, KC_K, KC_L, KC_SCLN, \
         KC_Z, KC_X, KC_C, KC_V, KC_B, KC_N, KC_M, KC_COMM, KC_DOT, KC_SLSH, \
         KC_ESC, KC_TAB, KC_LGUI, KC_LSFT, KC_BSPC, KC_LCTL, KC_LALT,   \
         KC_SPC, KC_FN0, KC_MINS, KC_QUOT, KC_ENT),                     \

  /* Numerics (Layer 1)
   * ,------------------------------------------------------------------------.
   * | !   | @   | up  | {   | }   |     ||     | pgup|   7 |   8 |   9 |   * |
   * |------------------------------------------------------------------------|
   * | #   |left |down |right| $   |     ||     | pgdn|   4 |   5 |   6 |   + |
   * |------------------------------------------------------------------------|
   * | [   | ]   | (   | )   | &   |     ||     |   ` |   1 |   2 |   3 |   \ |
   * |------------------------------------------------------------------------|
   * | L2  | ins |super|shift|bksp |ctrl ||  alt|space|  fn |   . |   0 |   = |
   * '------------------------------------------------------------------------'
   */

  KEYMAP(SHIFT(KC_1), SHIFT(KC_2), KC_UP, SHIFT(KC_LBRC), SHIFT(KC_RBRC), \
         KC_PGUP, KC_7, KC_8, KC_9, SHIFT(KC_8),                        \
         SHIFT(KC_3), KC_LEFT, KC_DOWN, KC_RIGHT, SHIFT(KC_4),          \
         KC_PGDN, KC_4, KC_5, KC_6, SHIFT(KC_EQUAL),                    \
         KC_LBRC, KC_RBRC, SHIFT(KC_9), SHIFT(KC_0), SHIFT(KC_7),       \
         KC_GRAVE, KC_1, KC_2, KC_3, KC_BSLS,                           \
         KC_FN1, SHIFT(KC_INS), KC_LGUI, KC_LSFT, KC_BSPC, KC_LCTL,     \
         KC_LALT, KC_SPC, KC_FN0, KC_DOT, KC_0, KC_EQUAL),              \

  /* Arrows and Function Keys
   * ,------------------------------------------------------------------------.
   * | ins |home | up  | end |pgup |     ||     | pgup|  F7 |  F8 |  F9 | F10 |
   * |------------------------------------------------------------------------|
   * | del |left |down |right|pgdn |     ||     | pgdn|  F4 |  F5 |  F6 | F11 |
   * |------------------------------------------------------------------------|
   * |mouse|volup|     |     |reset|     ||     |   ` |  F1 |  F2 |  F3 | F12 |
   * |------------------------------------------------------------------------|
   * |     |voldn|super|shift|bksp |ctrl ||  alt|space|  L0 |ptrsc|scrol|pause|
   * '------------------------------------------------------------------------'
   */
  KEYMAP(KC_INS, KC_HOME, KC_UP, KC_END, KC_PGUP,                       \
         KC_UP, KC_F7, KC_F8, KC_F9, KC_F10,                            \
         KC_DEL, KC_LEFT, KC_DOWN, KC_RIGHT, KC_PGDN,                   \
         KC_DOWN, KC_F4, KC_F5, KC_F6, KC_F11,                          \
         KC_NO, KC__VOLUP, KC_NO, KC_NO, KC_FN3,                        \
         KC_NO, KC_F1, KC_F2, KC_F3, KC_F12,                            \
         KC_NO, KC__VOLDOWN, KC_LGUI, KC_LSFT, KC_BSPC, KC_LCTL,        \
         KC_LALT, KC_SPC, KC_FN2, KC_PSCREEN, KC_SLCK, KC_PAUSE)
};

const uint16_t PROGMEM fn_actions[] = {
  [0] = ACTION_LAYER_MOMENTARY(1),  // to Fn overlay
  [1] = ACTION_LAYER_ON(2, 1),  // switch to layer 2
  [2] = ACTION_LAYER_OFF(2, 1),  // switch back to layer 0
  [3] = ACTION_FUNCTION(BOOTLOADER)
};

void action_function(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  if (id == BOOTLOADER) {
    bootloader();
  }
}
