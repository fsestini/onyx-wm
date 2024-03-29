#include <Carbon/Carbon.h>

bool get_isback(ProcessSerialNumber *psn) {
  ProcessInfoRec ProcessInfo = {};
  ProcessInfo.processInfoLength = sizeof(ProcessInfoRec);
  GetProcessInformation(psn, &ProcessInfo);
  return (ProcessInfo.processMode & modeOnlyBackground) != 0;
}

typedef OSStatus (carbon_event_callback_t)
  (EventHandlerCallRef HandlerCallRef, EventRef Event, void *Refcon);

EventHandlerUPP handler_upp(carbon_event_callback_t *callb) {
  return NewEventHandlerUPP(callb); }
