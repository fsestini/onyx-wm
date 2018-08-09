#include <Cocoa/Cocoa.h>

bool is_proc_hidden(pid_t PID, bool *result) {
  NSRunningApplication *app =
    [NSRunningApplication runningApplicationWithProcessIdentifier:PID];
  if (app) {
    *result = [app isHidden];
    return true;
  } else {
    return false;
  }
}

// Adapted from https://github.com/koekeishiya/chunkwm

char *proc_name_and_policy(pid_t PID, uint32_t *policy) {
  char *name = NULL;
  NSRunningApplication *app =
    [NSRunningApplication runningApplicationWithProcessIdentifier:PID];
  if (app) {
    *policy = [app activationPolicy];

    const char *appName = [[app localizedName] UTF8String];
    if (appName) { name = strdup(appName); }
  }

  return name;
}

@interface WorkspaceWatcher : NSObject { }
- (id)init;
@end

WorkspaceWatcher *Watcher;

void init_workspace() { Watcher = [[WorkspaceWatcher alloc] init]; }

pid_t notif_to_pid(NSNotification *notif) {
  return [[notif.userInfo objectForKey:NSWorkspaceApplicationKey]
	   processIdentifier];
}

typedef void (workspace_callback_t)(uint32_t, pid_t);
workspace_callback_t *workspace_callback;

void set_workspace_callback(workspace_callback_t *callb) {
  workspace_callback = callb;
}

@implementation WorkspaceWatcher
- (id)init {
  if ((self = [super init])) {
     [[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self
		selector:@selector(activeDisplayDidChange:)
		name:@"NSWorkspaceActiveDisplayDidChangeNotification"
		object:nil];

     [[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self
		selector:@selector(activeSpaceDidChange:)
		name:NSWorkspaceActiveSpaceDidChangeNotification
		object:nil];

     [[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self
		selector:@selector(didActivateApplication:)
		name:NSWorkspaceDidActivateApplicationNotification
		object:nil];

     [[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self
		selector:@selector(didDeactivateApplication:)
		name:NSWorkspaceDidDeactivateApplicationNotification
		object:nil];

     [[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self
		selector:@selector(didHideApplication:)
		name:NSWorkspaceDidHideApplicationNotification
		object:nil];

     [[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self
		selector:@selector(didUnhideApplication:)
		name:NSWorkspaceDidUnhideApplicationNotification
		object:nil];
  }

  return self;
}

- (void)dealloc {
    [[[NSWorkspace sharedWorkspace] notificationCenter] removeObserver:self];
    [super dealloc];
}

- (void)activeDisplayDidChange:(NSNotification *)notification {
  workspace_callback(0, 0);
}

- (void)activeSpaceDidChange:(NSNotification *)notification {
  workspace_callback(1, 0);
}

- (void)didActivateApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  workspace_callback(2, pid);
}

- (void)didDeactivateApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  workspace_callback(3, pid);
}

- (void)didHideApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  workspace_callback(4, pid);
}

- (void)didUnhideApplication:(NSNotification *)notification {
  pid_t pid = notif_to_pid(notification);
  workspace_callback(5, pid);
}

@end
