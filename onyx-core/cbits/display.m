#include <Cocoa/Cocoa.h>
#include <Carbon/Carbon.h>

void cgDisplayBounds_(uint32_t d_id, CGRect *rect)
{ *rect = CGDisplayBounds(d_id); }

typedef int CGSConnectionID;
typedef int CGSSpaceID;
typedef int CGSSpaceType;

typedef enum {
    kCGSSpaceCurrent = 5,
    kCGSSpaceOther = 6,
    kCGSSpaceAll = 7
} CGSSpaceSelector;

extern CGSConnectionID _CGSDefaultConnection(void);
#define CGSDefaultConnection _CGSDefaultConnection()

extern CFArrayRef CGSCopySpacesForWindows(
  CGSConnectionID Connection, CGSSpaceSelector Type, CFArrayRef Windows);

extern CFStringRef CGSCopyManagedDisplayForWindow(
  const CGSConnectionID Connection, uint32_t WindowId);

CGDirectDisplayID activeDisplay() {
  return [[[[NSScreen mainScreen] deviceDescription]
	    objectForKey:@"NSScreenNumber"] unsignedIntValue]; }

CFStringRef display_for_window(uint32_t window) {
  printf("def conn: %d\n", CGSDefaultConnection);
  return CGSCopyManagedDisplayForWindow(CGSDefaultConnection, window); }
CFArrayRef copy_spaces_for_windows(CGSSpaceSelector sel, CFArrayRef arr) {
  return CGSCopySpacesForWindows(CGSDefaultConnection, sel, arr); }

CFStringEncoding cfStringEncodingASCII() { return kCFStringEncodingASCII; }
CFAllocatorRef kCFAllocatorDefault_() { return kCFAllocatorDefault; }

const CFArrayCallBacks *cfCallbs() { return &kCFTypeArrayCallBacks; }
