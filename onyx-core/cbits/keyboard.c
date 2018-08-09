#include <stdbool.h>
#include <Carbon/Carbon.h>
#include <IOKit/hidsystem/ev_keymap.h>

extern bool CGSIsSecureEventInputSet();

int mykCGEventTapOptionDefault() { return kCGEventTapOptionDefault; }
int mykCGHeadInsertEventTap() { return kCGHeadInsertEventTap; }
int mykCGSessionEventTap() { return kCGSessionEventTap; }
uint32_t mykCGEventFlagMaskSecondaryFn() { return kCGEventFlagMaskSecondaryFn; }
CFAllocatorRef mykCFAllocatorDefault() { return kCFAllocatorDefault; }
CFStringRef mykCFRunLoopCommonModes() { return kCFRunLoopCommonModes; }

CGEventField kCGKeyboardEventKeycode_() { return kCGKeyboardEventKeycode; }

CGEventType kCGEventTapDisabledByTimeout_()
{ return kCGEventTapDisabledByTimeout; }
CGEventType kCGEventTapDisabledByUserInput_()
{ return kCGEventTapDisabledByUserInput; }
CGEventType kCGEventKeyDown_()
{ return kCGEventKeyDown; }

////////////////////////////////////////////////////////////////////////////////

// Adapted from https://github.com/koekeishiya/chunkwm

const void *unicodeLayoutData() { return kTISPropertyUnicodeKeyLayoutData; }

UniChar keyActionDown() { return kUCKeyActionDown; }

CFStringRef cfstring_from_keycode(CGKeyCode keycode) {
    TISInputSourceRef keyboard =
	TISCopyCurrentASCIICapableKeyboardLayoutInputSource();
    CFDataRef uchr =
	(CFDataRef) TISGetInputSourceProperty(
	    keyboard, kTISPropertyUnicodeKeyLayoutData);
    CFRelease(keyboard);

    UCKeyboardLayout *keyboard_layout = (UCKeyboardLayout *) CFDataGetBytePtr(uchr);
    if (keyboard_layout) {
	UInt32 dead_key_state = 0;
	UniCharCount max_string_length = 255;
	UniCharCount string_length = 0;
	UniChar unicode_string[max_string_length];

	OSStatus status = UCKeyTranslate(
	    keyboard_layout, keycode, kUCKeyActionDown, 0, LMGetKbdType(), 0,
	    &dead_key_state, max_string_length, &string_length, unicode_string);

	if (string_length == 0 && dead_key_state) {
	    status = UCKeyTranslate(
		keyboard_layout, kVK_Space, kUCKeyActionDown, 0, LMGetKbdType(),
		0, &dead_key_state, max_string_length, &string_length,
		unicode_string);
	}

	if (string_length > 0 && status == noErr) {
	    return CFStringCreateWithCharacters(
		NULL, unicode_string, string_length);
	}
    }

    return NULL;
}

uint32_t keycode_from_char(char key) {
    uint32_t keycode = 0;
    static CFMutableDictionaryRef keycode_map = NULL;
    if (!keycode_map) {
	keycode_map =
	    CFDictionaryCreateMutable(kCFAllocatorDefault,
				      128, &
				      kCFCopyStringDictionaryKeyCallBacks,
				      NULL);
	for (unsigned index = 0; index < 128; ++index) {
	    CFStringRef key_string = cfstring_from_keycode(index);
	    if (key_string) {
		CFDictionaryAddValue(keycode_map, key_string,
				     (const void *)index);
		CFRelease(key_string);
	    }
	}
    }

    UniChar uni_char = key;
    CFStringRef char_str =
	CFStringCreateWithCharacters(kCFAllocatorDefault, &uni_char, 1);
    CFDictionaryGetValueIfPresent(keycode_map,
				  char_str,
				  (const void **)&keycode);
    CFRelease(char_str);

    return keycode;
}
