#include <stdbool.h>
#include <Carbon/Carbon.h>
#include <IOKit/hidsystem/ev_keymap.h>
#include <stdlib.h>
#include <string.h>

// --------------------------------------------------------------------------------

typedef unsigned long (*table_hash_func)(void *key);
typedef int (*table_compare_func)(void *key_a, void *key_b);

struct bucket {
    void *key;
    void *value;
    struct bucket *next;
};
struct table {
    int count;
    int capacity;
    table_hash_func hash;
    table_compare_func compare;
    struct bucket **buckets;
};

static struct bucket * table_new_bucket(void *key, void *value) {
    struct bucket *bucket = malloc(sizeof(struct bucket));
    bucket->key = key;
    bucket->value = value;
    bucket->next = NULL;
    return bucket;
}

static struct bucket ** table_get_bucket(struct table *table, void *key) {
    struct bucket **bucket = table->buckets + (table->hash(key) % table->capacity);
    while (*bucket) {
        if (table->compare((*bucket)->key, key)) {
            break;
        }
        bucket = &(*bucket)->next;
    }
    return bucket;
}

void table_init(struct table *table, int capacity, table_hash_func hash, table_compare_func compare) {
    table->count = 0;
    table->capacity = capacity;
    table->hash = hash;
    table->compare = compare;
    table->buckets = malloc(sizeof(struct bucket *) * capacity);
    memset(table->buckets, 0, sizeof(struct bucket *) * capacity);
}

void *table_find(struct table *table, void *key) {
    struct bucket *bucket = *table_get_bucket(table, key);
    return bucket ? bucket->value : NULL;
}

void table_add(struct table *table, void *key, void *value) {
    struct bucket **bucket = table_get_bucket(table, key);
    if (*bucket) {
        if (!(*bucket)->value) {
            (*bucket)->value = value;
        }
    } else {
        *bucket = table_new_bucket(key, value);
        ++table->count;
    }
}

// --------------------------------------------------------------------------------

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

static char *
copy_cfstring(CFStringRef string)
{
    CFStringEncoding encoding = kCFStringEncodingUTF8;
    CFIndex length = CFStringGetLength(string);
    CFIndex bytes = CFStringGetMaximumSizeForEncoding(length, encoding);
    char *result = malloc(bytes + 1);

    // NOTE(koekeishiya): Boolean: typedef -> unsigned char; false = 0, true != 0
    Boolean success = CFStringGetCString(string, result, bytes + 1, encoding);
    if (!success) {
        free(result);
        result = NULL;
    }

    return result;
}

const void *unicodeLayoutData() { return kTISPropertyUnicodeKeyLayoutData; }

UniChar keyActionDown() { return kUCKeyActionDown; }

static CFStringRef cfstring_from_keycode(UCKeyboardLayout *keyboard_layout, CGKeyCode keycode)
{
    UInt32 dead_key_state = 0;
    UniCharCount max_string_length = 255;
    UniCharCount string_length = 0;
    UniChar unicode_string[max_string_length];

    OSStatus status = UCKeyTranslate(keyboard_layout, keycode,
                                     kUCKeyActionDown, 0,
                                     LMGetKbdType(), 0,
                                     &dead_key_state,
                                     max_string_length,
                                     &string_length,
                                     unicode_string);

    if (string_length == 0 && dead_key_state) {
        status = UCKeyTranslate(keyboard_layout, kVK_Space,
                                kUCKeyActionDown, 0,
                                LMGetKbdType(), 0,
                                &dead_key_state,
                                max_string_length,
                                &string_length,
                                unicode_string);
    }

    if (string_length > 0 && status == noErr) {
        return CFStringCreateWithCharacters(NULL, unicode_string, string_length);
    }

    return NULL;
}

static struct table keymap_table;

static int
hash_keymap(const char *a)
{
    unsigned long hash = 0, high;
    while (*a) {
        hash = (hash << 4) + *a++;
        high = hash & 0xF0000000;
        if (high) {
            hash ^= (high >> 24);
        }
        hash &= ~high;
    }
    return hash;
}

static bool
same_keymap(const char *a, const char *b)
{
    while (*a && *b && *a == *b) {
        ++a;
        ++b;
    }
    return *a == '\0' && *b == '\0';
}

bool initialize_keycode_map()
{
    TISInputSourceRef keyboard = TISCopyCurrentASCIICapableKeyboardLayoutInputSource();
    CFDataRef uchr = (CFDataRef) TISGetInputSourceProperty(keyboard, kTISPropertyUnicodeKeyLayoutData);
    CFRelease(keyboard);

    UCKeyboardLayout *keyboard_layout = (UCKeyboardLayout *) CFDataGetBytePtr(uchr);
    if (!keyboard_layout) return false;

    table_init(&keymap_table,
               131,
               (table_hash_func) hash_keymap,
               (table_compare_func) same_keymap);

    for (unsigned index = 0; index < 128; ++index) {
        CFStringRef key_string = cfstring_from_keycode(keyboard_layout, index);
        if (!key_string) continue;

        char *c_key_string = copy_cfstring(key_string);
        CFRelease(key_string);
        if (!c_key_string) continue;

        table_add(&keymap_table, c_key_string, (void *)index);
    }

    return true;
}

uint32_t keycode_from_char(char key) {
    char lookup_key[] = { key, '\0' };
    uint32_t keycode = (uint32_t) table_find(&keymap_table, &lookup_key);
    return keycode;
}
