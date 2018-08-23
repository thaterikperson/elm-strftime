# Strftime

An (incomplete) implementation of the strftime format based on rules
from [http://strftime.org](http://strftime.org).

## Usage

```elm
import Strftime exposing (format)

format "%d %B %y" Time.utc (Time.millisToPosix 1499000000000) == "02 July 17"

format "%B %d %Y, %-I:%M" Time.utc (Time.millisToPosix 1490000000000) == "March 20 2017, 8:53"
```
