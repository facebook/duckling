# Changelog for [`duckling` package](https://github.com/facebook/duckling)

## 0.2.0.0

### Core
  * Bump versions on dependencies
  * ExampleMain: add support for setting the port
  * Support for GHC >= 8.8.x && GHC <= 9.0.1
  * Relicense to BSD-3
  * Probabilistic layer bug fix
  * Remove dependency on Data.Some
  * Depend on regex-pcre-builtin on Windows
  * Update cabal-version to 2.2
  * Migrate to GitHub CI over Travis
  * Some attempts at getting smaller parse trees (less exploitable)

### Rulesets

* Common
  * Correct CDT TimeZone offset  CDT is (-300), not 540.
  * Fixed BST and IST offsets. BST is +1 (60) and IST is +5:30 (330)
  * Make unicode output in tests sane by not relying on `Show`
  * Add type=value to JSON response for Email, PhoneNumber, and Url, for consistency
  * CreditCardNumber: **new!** (duckling can now recognise credit cards)
  * Email: Add DE (German) + IS (Icelandic) spelled out email
  * Numeral: Don't accept dashes ('-') as token separators
  * Time: Add Karva Chauth holiday
  * Time: Add Vaisakhi holiday
  * Time: Add daylight savings start/endtimes to holidays
  * Time: Add Purim and Shushan Purim (Jewish holidays)
  * Time: Add Guru Gobind Singh Jayanti holiday
  * Time: Extend support for Ramadan and Eid al-Fitr from 1950 to 2050
  * Time: Add support for Parsi New Year
  * Time: Add support for Dayananda Saraswati Jayanti holiday
  * Time: Extend support for Mawlid from 1950 to 1998
  * Time: Add Rabindra Jayanti holiday
  * Time: Add Guru Ravidass Jayanti holiday
  * Time: Extend support for Eid al-Adha from 1950 to 2000
  * Time: Add Krishna Janmashtami holiday
  * Time: Add Mahavir Jayanti holiday
  * Time: Add Maha Shivaratri holiday
  * Time: add Ugadi holiday
  * Volume: interval support

* AF (Afrikaans)
  * Numeral: **new!**

* AR (Arabic)
  * Add a new locale: EG (Egyptian)
  * AmountOfMoney: Add more variants of EGP
  * Numeral: Add support for numerals written in Arabic script
  * Numeral: Support decimals and comma-separated integers
  * PhoneNumber: **new!**
  * Time: Add periodic times
  * Time: Add rule for a week ago

* BG (Bulgarian)
  * Time: **new!**

* BN (Bengali)
  * Numeral: **new!**

* CA (Catalan)
  * Numeral: **new!**
  * Ordinal: **new!**

* DA (Danish)
  * Ordinal: Add support for larger spelled-out ordinals
  * Time: recognise abbreviation 'kl'

* DE (German)
  * Distance: **new!**
  * Duration: Add more common durations
  * Numeral: Fix typo on "fünfzehn"(15)
  * Time: Fix wrong parsing of YYYY-MM-DD dates
  * Time: Add rule for 'the day before yesterday'
  * Time: Fix a bug for "fünfter"¬
  * Time: support for "am ersten Dezember" to " am einunddreißigsten Dezember"¬
  * Time: now recognise "der fünfte Dezember"¬
  * Time: Add support for (pre-)computed holidays
  * Time: Don't parse 'so'
  * Time: improve approximations
  * Time: Add rule for week after next
  * Time: Add rule for montag in n wochen
  * Volume: **new!**

* EN (English)
  * AmountOfMoney: Add support for subunits of dollars, e.g. nickels, dimes, quarters, as well as the number of coins
  * AmountOfMoney: Add support for latent
  * AmountOfMoney: Add support for lakh and crore
  * AmountOfMoney: correct regex to match three letter currency codes beginning with C
  * AmountOfMoney: Make ruleInterval{Max,Min} symmetric
  * AmountOfMoney: Support abbreviations of lakh and crore
  * AmountOfMoney: Add UAH currency
  * AmountOfMoney: Add HKD
  * AmountOfMoney: dollar and \<amount-of-money\>
  * AmountOfMoney: dollar and a half
  * AmountOfMoney: intersection for \<amount-of-money\> and a half
  * AmountOfMoney: extend interval support
  * AmountOfMoney: Add more variants of EGP
  * Distance: Support composite distances
  * Distance: one meter and \<distance\>
  * Distance: \<distance\> meters and \<distance\>
  * Distance: Add interval support
  * Duration: Fix composite durations without delimiters.
              It previously only worked with
              commas/'and'-separated tokens
  * Duration: Support more words in `ruleDurationNumeralMore`
  * Duration: \<integer\> and a half minutes
  * Duration: support for \<integer\> hour and \<integer\>
  * Duration: parse Xm as X minutes and X.Y hrs as X.Y hours
  * Duration: Leverage TimeGrain for x.y hours
  * Duration: Support composite duration
  * Numeral: Fix ambiguous parses when both ruleNegative and ruleMultiply apply
  * Numeral: Extend fraction rule
  * Numeral: Add mixed fraction rules
  * Numeral: Add prefix of 10/100/10_000 rules
  * Quantity: Support 'k.g' and 'k.g.' for kilograms
  * Temperature: Support temperatures with decimal values
  * Time: Fix palm sunday regex
  * Time: Fix latent time of days like "ten thirty"
  * Time: Don't parse time of days above 12 with meridiem
          like "13 am"
  * Time: Add rule to match \<time\> + \<timezone\>
  * Time: Add "all week"/"rest of the week" rules
  * Time: Parse time intervals like: 2015-03-28 17:00:00/2015-03-28 21:00:00
  * Time: Add rules to handle "DD/MM/YYYY" or "DD MM YYYY"
  * Time: Add support for YYYYQQ and YYQQ expressions, like
          '2018Q4' and '18Q4'
  * Time: Add rule for 'during \<month\>'
  * Time: Restrict 'on' absorption to days
  * Time: Fix durations, upper intervals should all be exclusive
  * Time: Fix grain on some intervals for time-of-days
  * Time: Add rule 'in \<duration\> at \<time-ofday\>
  * Time: Add \<datetime\> - \<datetime\> (interval) timezone rule
  * Time: '\<day\> in \<duration\> should only operate on grain \> Hour
  * Time: Add '\<integer\> \<day-of-week\> from \<time\>' rule
  * Time: Add \<duration\> past \<time\>
  * Time: Make 'the week' resolve to interval from `today` to `end of week`
  * Time: Add support for \<hour\>h\<min\>
  * Time: Add rule for a quarter after \<hour-of-day\>
  * Time: Support 'Martin Luther Kings Day'
  * Time: Add more holiday aliases (Veteran Day, Mardi Gras, St. Paddy's Day)
  * Time: Intersect "9 tomorrow morning"
  * Time: Support 'Chinese New Years'
  * Time: Support "Saint" Patrick's Day
  * Time: Add Ratha-Yatra Holiday
  * Time: Add support for Ganesh Chaturthi holiday
  * Time: Add rama navami holiday
  * Time: Add '\<part-of-day\> at \<time-of-day\>' rule
  * Time: Modify 'this|last|next \<cycle\>' to capture things like "the month" in "the first Saturday of the month"
  * Time: Support 'the \<day-of-month\> of \<month\>'
  * Time: Add Orthodox Good Friday holiday
  * Time: The (nth) closest (day) to (time)
  * Time: Add rule for 'midday'
  * Time: Fix a problem with parsing fractional minutes
  * Time: parse 'upcoming xxx weeks' and 'xxx upcoming weeks'
  * Time: Add new rule to parse phrase in the pattern 'xxx minutes to \<hour-of-day\>
  * Time: Add support for spelled-out times
  * Time: Add support to parse \<ordinal\> \<day-of-week\>
  * Time: Fix parsing 'next \<day-of-week\>'
  * Time: Treat 'coming' as 'next'
  * Time: Make duckling time not treat 0:xx and 12:xx ambiguously
  * Add 'asap', 'at the moment' to EN time
  * Parse latent year intervals
  * Time: last \<duration\>
  * Time: \<time\> \<day-of-month\>
  * Volume: Extend interval support
  * Time: Handle 2-digit date in existing DD/MM/YYYY rule

* EN_UK (English, United Kingdom)
  * Duration: **new!**
  * Time: **new!**

* EN_US (English, United States)
  * Time: Super Tuesday

* ES (Spanish)
  * AmountOfMoney: Add support for intervals
  * Duration: Add support for parsing phrases like half hour, quarter of an hour
  * Duration: Support composite durations
  * Numeral: Fix typo of 22 (veinto -> venti)
  * Numeral: Add locale rules, since Spain and South America use different decimal separators
  * Numeral: parse 'cero dos' as 'dos'
  * Numeral: composite numerals
  * Ordinal: Add missing 'tercer' regex
  * Quantity: **new!**
  * Time: Add support for periodic holidays
  * Time: Fix `ruleYearLatent` to not match numerals that could be hours
  * Time: Make 'n horas' latent
  * Time: Fixed a problem with parsing 'day of month' that contains 'dia'
  * Time: Add rule for 'next week'
  * Time: \<ordinal\> \<day-of-month\>
  * Time: support for 'noon'
  * Time: Allow para before timestamps
  * Time: Add rule to parse '\<time-of-day\> (in the afternoon)'
  * Time: Add rule to parse 'last \<day-of-week\> of \<time\>'
  * Time: Add rule for next week
  * Time: Add more corpus for tomorrow so that tomorrow latent doesn't prevail
  * Time: Fix multi-word timestamps

* FA (Persian)
  * Numeral: **new!**

* FI (Finnish)
  * Numeral: **new!**

* FR (French)
  * Temperature: Support temperatures with decimal values
  * Time: Update month interval rules to handle ordinals
          and spelled-out numerals (e.g. "du premier au
          quinze juin")
  * Volume: Add 'Centilitre' type

* HE (Hebrew)
  * AmountOfMoney: add more rules

* HI (Hindi)
  * Duration: Pakhwada (पखवाड़ा) is 15 days n, not a fortnight
  * Duration: Add support for composite duration
  * Duration: Add support for specific times
  * Numeral: Support more numerals
  * Temperature: **new!**
  * Time: Add support for quarter till, quarter past, and half

* ID (Indonesian)
  * AmountOfMoney: support intervals

* IS (Icelandic)
  * Numeral: **new!**

* IT (Italian)
  * AmountOfMoney: **new!**
  * Distance: **new!**
  * Duration: Fix ruleDurationAgo

* KA (Georgian)
  * AmountOfMoney: **new!**
  * Duration: **new!**
  * Numeral: **new!**
  * Ordinal: **new!**
  * Time: **new!**
  * Time: Improvements for times in the past
  * Time: Add support for quarters

* KM (Khmer)
  * Distance: **new!**
  * Numeral: **new!**
  * Ordinal: **new!**
  * Quantity: **new!**
  * Temperature: **new!**
  * Volume: **new!**

* KN (Kannada)
  * Numeral: **new!**

* KO (Korean)
  * AmountOfMoney: support intervals

* LO (Lao)
  * Numeral: **new!**

* ML (Malayalam)
  * Numeral: **new!**
  * Ordinal: **new!**

* MN (Mongolian)
  * Numeral: **new!**

* NL (Dutch)
  * AmountOfMoney: make decimal thousands separator optional
  * Duration: Add 'anderhalf uur'
  * Duration: support composite durations
  * Quantity: **new!**
  * Time: Add support for King's Day (Koningsdag)
  * Time: stop 'for \<number\>' from resolving as times
  * Volume: remove gallon
  * Volume: Fix typo in milliliter

* NO (Norwegian)
  * AmountOfMoney: Parse more currencies
  * Numeral: The written numeral 8 had a typo: "otte" -> "åtte"
  * Numeral: Parse powers of ten with spaces as well as dots
  * Numeral: Add more textual powers of ten
  * Numeral: Parse textual numbers from 21 to 99 with _and_ without spaces
  * Time: Add support for half an hour before as e.g. "halv to"
  * Time: Add support for alternative clock denotation "klokka"
  * Time: Add support for alternative tomorrow denotation "i morra"
  * Time: Add some more unambiguous datetime parsing

* PL (Polish)
  * Numeral: Add support for seventy, eighty, ninety
  * Duration: Fix typo: miej -> mniej
  * Time: Add 'evening' to corpus

* PT (Portuguese)
  * Ordinal: Add 13..99
  * Ordinal: Corpus, keep accents consistent
  * Time: Last and quarter expressions

* RO (Romanian)
  * Numeral: Fix multipliers with values above 20
             In Romanian, for numerals above 20, we
             say '20 de milioane', not '20 milioane'.
  * Time: Implement 'the day after tomorrow'

* RU (Russian)
  * AmountOfMoney: Add UAH currency
  * Numeral: Be more permissive with numerals [20, 90]
  * Time: **new!**

* SK (Slovak)
  * Numeral: **new!**

* SW (Swahili)
  * Numeral: **new!**

* TA (Tamil)
  * Ordinal: **new!**

* TE (Telugu)
  * Numeral: **new!**

* TH (Thai)
  * Numeral: **new!**

* TR (Turkish)
  * AmountOfMoney: **new!**
  * Time: **new!**

* VI (Vietnamese)
  * AmountOfMoney: Add interval support
  * AmountOfMoney: ruleNg, ruleDollar, ruleVND modified to better capture usage of VI
  * Numeral: Add "ngàn" common synonym of "nghìn", and "chục",
             colloquially used to count tens.
  * Numeral: Remove ? in some regex where they don't make sense
  * Time: Fix double-digit month matching
  * Time: don't parse ngày

* ZH (Chinese)
  * Duration: Add more common expressions
  * Numeral: Add more common expressions
  * Time: Parse YYYY-MM
  * Time: Value before month can be integer or chinese char
  * Time: Add support for (pre-)computed holidays
  * Time: Add more common expressions
  * Volume: **new!**

### Server
  * Skip logfile creation if no logging
  * Document how example application can use specific dimensions only
  * Use System.FilePath.Posix instead of System.FilePath
  * Use all dimensions by default
  * Load timezones more leniently
  * Docker: Reduce size of image drastically
  * Dockerfile: Use Debian Buster

## 0.1.6.1

### Core

* Warn against incomplete pattern matching in GHC options
* Updated readme for custom dimensions
* Coding style documentation

### Rulesets

* Common
  * AmountOfMoney: change `financeWith` to `isAmountOfMoney` where applicable
  * Temperature: implement temperature intervals

* EN
  * AmountOfMoney: support "grand"
  * Time: Absorb "since" before month/year
  * Time: Fix before/after/until/since + <year>
  * Time: Make partial meridies latent
  * Time: Global Youth Service Day
  * Time: Vesak
  * Time: Earth Hour
  * Time: Regex update for Rosh Hashanah and Chanukkah
  * Time: add <time> for <duration> rules
  * Distance: Millimeter units and UK spellings
  * Duration: Handle half time grains

* EN_US
  * Time: Fix Emancipation Day
  * Time: Tax Day
  * Time: Siblings Day
  * Time: EMS Week and EMSC Day

* EN_AU
  * Time: Ekka

* EN_XX
  * Time: Administrative Professionals' Day
  * Time: Parse 'second last week of October 2018'
  * Time: AD/BC parsing for years

* AR
  * Numeral: Fix dual big numbers
  * Numeral: Fix million

* DA_XX
  * Time: Remove 'man'

* DE
  * Time: Remove am/pm rule

* HI
  * Duration: **new!**
  * TimeGrain: **new!**

* NL
  * Time: Fix Sinterklaas for Belgium

* RO
  * AmountOfMoney: Fix for values above 20
  * AmountOfMoney: Support intervals
  * Distance: Fix for values above 20
  * Duration: Fix for values above 20
  * Quantity: Fix for values above 20
  * Temperature: Fix for values above 20
  * Volume: Fix for values above 20

* ZH
  * Quantity: **new!**
  * Time: Support Periodic Holidays

## 0.1.6.0

### Core

The `value` field of `Entity` is now typed! Use `toJText` to get the JSON-encoded string back.

## 0.1.5.0

### Testing
* Prompt ambiguous parses in corpus tests

### Rulesets

* Common
  * Add an option to return latent time entities.
  * AmountOfMoney: Distribute ruleUnitAmount to all languages
  * Time: Make all years latent
  * Lambda-case for Rule production
  * Numeral: fix intersect rule to work with negative numbers
  * Support custom dimensions

* BG_XX:
  * Distance: **new!**
  * TimeGrain: **new!**
  * Duration: **new!**
  * Ordinal: **new!**

* EN_XX
  * Time: periodic holidays
  * Time: handle Easter
  * Time: parse "DD MM YYYY", "MM DD YYYY"  as date instead of only phone fix
  * Time: added rules for the beginning / end of year / month / week
  * Time: "late tonight"
  * Time: "last night" and "late last night"
  * Time: fixes in holidays
  * Time: Return Holiday Name in response
  * Duration: composite
  * Time: support for this|last|next season
  * Time: added Easter-based holidays
  * Time: precomputed Orthodox Easter dates
  * Time: added Orthodox Easter-based holidays
  * Time: fix Labour Day, add Heroes' Day and National Patriots' Day
  * Time: Chinese New Year
  * Time: added Boss's Day
  * Time: fix computed past series and Jewish holidays
  * Time: added rules to cover EOY, EOM, BOY, BOM
  * Time: rule <Day of month> of month
  * Time: Islamic holidays
  * Time: fix "in" + year
  * Time: Added precomputed Jewish holiday "Tu BiShvat"
  * Time: time + n years ago/in n years
  * Time: make "may" latent
  * Duration: move composite duration rule to English
  * Time: add Diwali, Navaratri, Vasant Panchami, Holika Dahan, Holi and other Hindu holidays

* EN_AU
  * Time: add Reconciliation Day

* EN_CA
  * Time: add Discovery day
  * Time: Add "Groundhogs day" in regex for "Groundhog Day"

* EN_GB
  * Time: fixed mother's day

* EN_US
  * Time: Washington's birthday, President day and Lincoln's birthday
  * Time: Add "Groundhogs day" in regex for "Groundhog Day"

* ES_XX
  * Time: use `mkRuleSeasons`

* DE_XX
  * Time: return Holiday Name in response

* NL_XX
  * Time: return Holiday Name in response
  * AmountOfMoney: **new!**

* PL_XX
  * Time: month regex fix

* RU_XX
  * Ordinal: refactor pattern match to hashmap lookup

* SV_XX
  * Distance: **new!**
  * Distance: fix mil

* TA_XX
  * Numerals: **new!**

* VI_XX
  * AmountOfMoney: Use comma "," as decimal separator

* ZH_XX
  * Distance: **new!**
  * Time: remove unused `ruleLastTuesdayLastJuly`
  * Numeral/Distance/Time: generalize and expand digit specifier usage
  * AmountOfMoney: **new!**

## 0.1.4.0

### Core
* Bumped to LTS 9.10
* API: Added `Node` to `Entity` to return the parse tree

### Rulesets
* Common
  * AmountOfMoney: don't recursively compare cents
  * AmountOfMoney: don't allow decreasing intervals
  * Duration: added `type` in response
  * Numeral: common rule to parse numbers
  * Numeral: common rule to parse fractions
  * Numeral: don't compose negative numbers
  * Time: fixed empty values for past datetimes
  * Time: fixed case sensitivity for timezones
  * Url: fragments support
* AR_XX
  * AmountOfMoney: **new!**
  * Duration: **new!**
  * Ordinal: more rules
  * Quantity: **new!**
  * Temperature: **new!**
  * Time: **new!**
  * Volume: **new!**
* DE_XX
  * Numeral: fixed keiner
  * Time: fixed this/next/last for holidays/months/days of week/seasons/weekend only
* EL_XX
  * Duration: **new!**
  * Numeral: **new!**
  * Ordinal: **new!**
  * Time: **new!**
* EN_GB
  * Time: fix parsing Oct-Dec months
* EN_XX
  * AmountOfMoney: intervals support
  * Distance: intervals support
  * Email: spelled out 'dot'
  * Numeral: prevent double negatives
  * Numeral: skip hundred for alphabetical numbers only
  * Quantity: intervals support
  * Time: don't parse "at" + phone number
  * Time: fixed date format in non-US locales
  * Time: fixed Thanksgiving in locales
  * Time: don't parse "this in 2 minutes"
  * Time: spelled out times + AM/PM
  * Time: fixed case sensitivity
  * Time: fixed "from" + time of day
  * Time: support MM.DD.YYYY
* FR_XX
  * Numeral: space as thousand separator
* GA_XX
  * Numeral: fix old vigesimal
* HI_XX
  * Numeral: **new!**
  * Ordinal: **new!**
* JA_XX
  * Time: removed dimension, as it is not implemented yet
* NE_XX
  * Numeral: **new!**
* NL_XX
  * Time: **new!**
  * Time: don't be too eager on days of week
  * Time: today/tomorrow/yesterday evening/afternoon/morning
  * Time: fixed this/next/last for holidays/months/days of week/seasons/weekend only
* PT_XX
  * Numeral: fixed rules
* RU_XX
  * AmountOfMoney: **new!**
  * Distance: **new!**
  * Duration:: **new!**
  * Numeral: 1.5 rule, more 0 rules, fixed 11, 300 and 400
  * Ordinal: fixes
  * Quantity: **new!**
  * Volume: **new!**
* SV_XX
  * Duration: extend support for "about" + duration

### Server
* Fixed Dockerfile
* Added reference time parameter

## 0.1.3.0

### Core
* Support locales
* Don't allow matches in the middle of words

### Rulesets
* Common
  * All: converted back all escaped characters to unicode
  * Time: don't shift duration on "`Duration` before/after `Time`"
  * Time: don't parse subsequent numbers
  * Time: fix `nthDOWOfMonth`
  * Time: reuse the `weekend` helper in all languages
* BG_XX
  * AmountOfMoney: **new!**
  * Numeral: **new!**
* DA_XX
  * Time: Consolidated rules for months and days of week
* DE_XX
  * Time: don't parse "nächste 5"
  * Time: Consolidated rules for months and days of week
  * Time: Consolidated rules for holidays, instants and seasons
* EN_CA
  * Time: Fixed Thanksgiving Day
* EN_GB
  * Time: Fixed DDMM and YYYYDDMM
* EN_XX
  * Time: "for `Duration` from `Time`"
  * Time: "from 10 to 16 August"
  * Time: "August 27th-30th"
  * Time: "23rd to 26th Oct", "1-8 september"
  * Time: Support misspelled "tonights" and "weekends"
  * Quantity: Support ounces
  * Email: restrict domain extensions to letters when spelling out
* ES_XX
  * Time: Consolidated rules for months and days of week
  * Ordinal: Fixed "first" variants + accents
* FR_XX
  * Time: don't parse "a un"
  * Time: Consolidated rules for months and days of week
* GA_XX
  * Time: Consolidated rules for months and days of week
* HE_XX
  * Time: Consolidated rules for months
* HR_XX
  * Time: Consolidated rules for months and days of week
* HU_XX
  * Duration: **new!**
  * Numeral: **new!**
  * Ordinal: **new!**
  * Time: **new!**
* IT_XX
  * Time: Consolidated rules for months and days of week
* JA_XX
  *Numeral: Optimized regex lookup
* KA_XX
  * Numeral: **new!**
* NB_XX
  * Time: Consolidated rules for months and days of week
* NL_XX
  * Duration: **new!**
* PL_XX
  * Time: don't parse "nie"
  * Time: don't parse ordinals without context
  * Time: Consolidated rules for months and days of week
* PT_XX
  * Time: don't parse "um" alone
  * Time: don't parse "ter"
* RO_XX
  * Time: Consolidated rules for months and days of week
  * Time: don't parse "sa"
* SV_XX
  * Ordinal: Fixed ordinals
  * Time: "i + day-of-week"
  * Time: Consolidated rules for months and days of week
* VI_XX
  * Time: Consolidated rules for months and days of week
* ZH_TW
  * Time: Fixed National Day
* ZH_XX
  * Time: Consolidated rules for months and days of week

## 0.1.2.0

### Core

* `weekend` production helper
* Numeral flag for Time patterns

### Rulesets

* FR
  * Time
    * `début|milieu|fin de matinée` (`early|mid|late morning`)
    * `début|milieu|fin de après-midi` (`early|mid|late afternoon`)
    * `début|milieu|fin de journée` (`early|mid|late day`)
    * `début|fin de soirée` (`early|late evening`)
    * `début|fin de mois` (`early|late month`)
    * `début|fin d'année` (`early|late year`)
    * `plus tard` (`later`)
    * `plus tard que ...` (`later than ...`)
    * `plus tard <time>` (`later than <time>`)
    * `plus tard <part-of-day>` (`later than <part-of-day>`)
* EN
  * Time
    * Support dates of type mm-dd
    * Fix issue #50 "last weekend of october"
    * Added a rule to handle "from <month> dd-dd"
    * Add MM/YYYY
* DE
  * Time
    * Make last dot optional in "13.03."
    * Handle variations of "13.-15.10." correctly
    * Treat "14/15Uhr" the same as "14-15Uhr"
    * Extended "bis" to also match "bis zum" and "auf den"
    * Changed `hh:mm` matching to also get the expression "17h00"
* PT
  * Numeral
    * Numerals between 100 and 999 in Portuguese fixed
* GA
  * Ordinal
    * Also match old-style -adh ending

## 0.1.1.0

### Core
* More optimizations for `Time` internals.
* `Entity` value is now a `Value` (was a JSON-encoded `Text`).

### Rulesets
* Common
  * AmountOfMoney: MYR currency
  * Duration: Helper for "<numeral>.<numeral> hours"
* CS
  * Distance: **new!**
  * Numeral: **new!**
* DE
  * Ordinal: Don't parse "1.1."
  * Time: Small improvements
* EN
  * AmountOfMoney
    * Support pence as cents
    * Support Malaysian ringgits
  * Distance: Abbreviations for inches, miles
  * Numeral: Fractions
  * Quantity: Generic product
  * Time
    * Allow spaces in e.g. "2 / 15"
    * Afternoonish
    * Early/mid/late month
* ID
  * Numeral: Optimization for regex matching
* KO
  * Time: Fixed typo in the day after tomorrow
* SV
  * Time: vid
* TR
  * Distance: **new!**
  * Duration: **new!**
  * Numeral: Comma as decimal mark
  * Temperature: **new!**
  * Volume: **new!**
* ZH
  * Time: Cantonese support

### Server
* Added dockerfile
* Fixed warnings around logs

## 0.1.0.0

* Initial version. Released on an unsuspecting world.
