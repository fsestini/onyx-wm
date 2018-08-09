#include <Carbon/Carbon.h>

#define kAXValueTypeCGPoint 1
#define kAXValueTypeCGSize 2

CFStringRef ax_ui_element_destroyed_notification() {
  return kAXUIElementDestroyedNotification; }
CFStringRef ax_window_miniaturized_notification() {
  return kAXWindowMiniaturizedNotification; }
CFStringRef ax_window_deminiaturized_notification() {
  return kAXWindowDeminiaturizedNotification; }

CFStringRef window_created_notif() { return kAXWindowCreatedNotification; }
CFStringRef focused_window_changed_notif() {
  return kAXFocusedWindowChangedNotification; }
CFStringRef window_moved_notif() { return kAXWindowMovedNotification; }
CFStringRef window_resized_notif() { return kAXWindowResizedNotification; }
CFStringRef title_changed_notif() { return kAXTitleChangedNotification; }

CFStringRef kCFRunLoopDefaultMode_() { return kCFRunLoopDefaultMode; }

AXError error_invalid_element_observer() {
  return kAXErrorInvalidUIElementObserver; }
AXError error_illegal_argument() { return kAXErrorIllegalArgument; }
AXError error_notification_unsupported() {
  return kAXErrorNotificationUnsupported; }
AXError error_notification_already_registered() {
  return kAXErrorNotificationAlreadyRegistered; }
AXError error_cannot_complete() { return kAXErrorCannotComplete; }
AXError error_failure() { return kAXErrorFailure; }
AXError error_success() { return kAXErrorSuccess; }

int cgErrorCannotComplete() { return kCGErrorCannotComplete; }
int cgErrorFailure() { return kCGErrorFailure; }
int cgErrorIllegalArgument() { return kCGErrorIllegalArgument; }
int cgErrorInvalidConnection() { return kCGErrorInvalidConnection; }
int cgErrorInvalidContext() { return kCGErrorInvalidContext; }
int cgErrorInvalidOperation() { return kCGErrorInvalidOperation; }
int cgErrorNoneAvailable() { return kCGErrorNoneAvailable; }
int cgErrorNotImplemented() { return kCGErrorNotImplemented; }
int cgErrorRangeCheck() { return kCGErrorRangeCheck; }
int cgErrorSuccess() { return kCGErrorSuccess; }
int cgErrorTypeCheck() { return kCGErrorTypeCheck; }

typedef int CGSConnectionID;
extern CGSConnectionID _CGSDefaultConnection(void);
extern CGError CGSGetWindowLevel(
  const CGSConnectionID Connection, uint32_t WindowId, uint32_t *WindowLevel);

CFStringRef ax_size_attribute() { return kAXSizeAttribute; }
CFStringRef ax_window_role() { return kAXWindowRole; }
CFStringRef ax_standard_window_subrole() { return kAXStandardWindowSubrole; }
CFStringRef ax_windows_attribute() { return kAXWindowsAttribute; }
CFStringRef ax_minimized_attribute() { return kAXMinimizedAttribute; }
CFStringRef ax_position_attribute() { return kAXPositionAttribute; }

const CFDictionaryKeyCallBacks *cfCopyStringDictionaryKeyCallBacks() {
  return &kCFCopyStringDictionaryKeyCallBacks; }
const CFDictionaryValueCallBacks *cfTypeDictionaryValueCallBacks() {
  return &kCFTypeDictionaryValueCallBacks; }

CFStringRef axTrustedCheckOptionPrompt() { return kAXTrustedCheckOptionPrompt; }

CFStringEncoding uif8enc() { return kCFStringEncodingUTF8; }

CFStringRef ax_focused_window_attribute() { return kAXFocusedWindowAttribute; }
CFStringRef ax_main_attribute() { return kAXMainAttribute; }
CFStringRef ax_focused_attribute() { return kAXFocusedAttribute; }

CFBooleanRef cf_boolean_true() { return kCFBooleanTrue; }
CFBooleanRef cf_boolean_false() { return kCFBooleanFalse; }

CFTypeRef create_cfpoint(CGPoint *point) {
  return (CFTypeRef) AXValueCreate(kAXValueTypeCGPoint, (void *)point); }
CFTypeRef create_cfsize(CGSize *size) {
  return (CFTypeRef) AXValueCreate(kAXValueTypeCGSize, (void *)size); }

CFStringRef ax_press_action() { return kAXPressAction; }
CFStringRef ax_fullscreen_attribute() { return CFSTR("AXFullScreen"); }
CFStringRef ax_close_button_attribute() { return kAXCloseButtonAttribute; }

CFStringRef ax_title_attribute() { return kAXTitleAttribute; }

Boolean ax_value_get_cgpoint(AXValueRef value, void *valuePtr) {
  return AXValueGetValue(value, kAXValueTypeCGPoint, valuePtr); }

Boolean ax_value_get_cgsize(AXValueRef value, void *valuePtr) {
  return AXValueGetValue(value, kAXValueTypeCGSize, valuePtr); }

CFStringRef ax_role_attribute() { return kAXRoleAttribute; }
CFStringRef ax_subrole_attribute() { return kAXSubroleAttribute; }
