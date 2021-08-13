### 1.8.0 - August 12 2021
* Compiles without generating closing tags #17

### 1.7.0 - December 15 2018
* Fixed #12 - (thanks @petejohanson)

### 1.6.0 - June 28 2018
* Added fromFileSafe function
* Added fromTextSafe function

### 1.5.1 - September 25 2017
* Fixed problems with Option values boxed to null

### 1.5.0 - September 06 2017
* Move to .NET Standard 2.0

### 1.4.0 - August 14 2017
* Added $is-last and $is-not-last values for loops (thanks @otto-geb!)
* Fixed quadratic complexity of loops (thanks @otto-geb!)

### 1.3.1 - August 11 2017
* Fix incorrect deletion of the end "param" tag (Issue #8)

### 1.3.0 - May 29 2017
* Fix for literals with spaces (Issue #4)
* Supports using numbers in templates (Issue #5)
* Improved parsing logic

### 1.2.0 - March 10 2017
* Added support for direct tuples destructuring in fs-for cycle
 
### 1.1.1 - February 03 2017
* Bugfix for parsing piped functions with literals

### 1.1.0 - January 17 2017
* Output now keeps original case (XAML support)

### 1.0.0 - January 10 2017
* Public release

### 0.9 - January 09 2017
* Fallback to old implementation (performace reasons)

### 0.8.2 - January 09 2017
* Added Async method for compiling from file

### 0.8.1 - January 04 2017
* Fix for HtmlAgilityPack drops end of option tags

### 0.8.0 - January 04 2017
* Constant renamed to Literal in core
* Better parsing

### 0.7.0 - January 04 2017
* Added fs-template tag

#### 0.6.0 - January 03 2017
* Added constant value type

#### 0.5.2 - January 02 2017
* Different bugfix for fs-else

#### 0.5.1 - January 02 2017
* Bugfix for fs-else

#### 0.5.0 - January 02 2017
* Attribute fs-if-not removed in favor of fs-else

#### 0.4.4 - December 20 2016
* Discriminated Union can ignore extractions

#### 0.4.3 - December 20 2016
* Bugfix for properties on Option None values

#### 0.4.2 - December 20 2016
* Bugfix for Option values

#### 0.4.1 - December 20 2016
* Bugfix for complex html
 
#### 0.4.0 - December 19 2016
* Added fs-if-not attribute
* Added for cycle auto-created values
 
#### 0.3.0 - December 19 2016
* FSharp.Data removed in favor of HtmlAgilityPack

#### 0.2.0 - December 19 2016
* Compiler API change

#### 0.1.0 - December 19 2016
* Initial release - beta