---
title:      Karabiner Configuration
date:       2026-11-12T13:26:56+01:00
tags:       emacs
identifier: 
signature:  
---

For a `hyper` modifier via caps-lock, I have the following Karabiner advanced configuration: 
```
{
    "manipulators": [
        {
            "description": "Change caps_lock to right_control",
            "from": {
                "key_code": "caps_lock",
                "modifiers": { "optional": ["any"] }
            },
            "to": [{ "key_code": "right_control" }],
            "type": "basic"
        }
    ]
}
```

I had an idea to swap the left control and shift keys to reduce the pinky travel when in Emacs:

```
{
    "manipulators": [
        {
            "description": "Swap left_control and left_shift",
            "from": {
                "key_code": "left_control",
                "modifiers": { "optional": ["any"] }
            },
            "to": [{ "key_code": "left_shift" }],
            "type": "basic"
        },
        {
            "description": "Swap left_control and left_shift",
            "from": {
                "key_code": "left_shift",
                "modifiers": { "optional": ["any"] }
            },
            "to": [{ "key_code": "left_control" }],
            "type": "basic"
        }
    ]
}
```

However, this requires too much retraining to be useful, so it is now disabled.
