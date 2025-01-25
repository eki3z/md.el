---
title: "Example Post"
date: 2023-10-15
author: "John Doe"
tags:
  - markdown
  - yaml
  - example
taggs: ["markdown", "toml", "example"]
draft: false
---

# Markdown Grammar Guide

## Headers

# This is an H1
## This is an H2
### This is an H3
#### This is an H4
##### This is an H5
###### This is an H6

---

<!-- md-toc start - Don't edit this section. Run M-x md-toc-update-doc -->

**Table of Contents**

- [Markdown Grammar Guide](#markdown-grammar-guide)
   - [Headers](#headers)
- [This is an H1](#this-is-an-h1)
   - [This is an H2](#this-is-an-h2)
      - [This is an H3](#this-is-an-h3)
         - [This is an H4](#this-is-an-h4)
            - [This is an H5](#this-is-an-h5)
               - [This is an H6](#this-is-an-h6)
         - [todo](#todo)
      - [My Great Heading](#my-great-heading)
   - [Emphasis](#emphasis)
   - [Lists](#lists)
      - [Unordered list:](#unordered-list)
      - [Ordered list:](#ordered-list)
   - [Links](#links)
   - [Images](#images)
   - [Blockquotes](#blockquotes)
   - [Code](#code)
      - [Inline code:](#inline-code)
      - [Code block:](#code-block)
         - [fenced code block](#fenced-code-block)
         - [indented code block](#indented-code-block)
   - [Tables](#tables)
   - [Horizontal Rules](#horizontal-rules)
   - [Tasks Lists](#tasks-lists)
   - [Escaping Characters](#escaping-characters)
   - [HTML in Markdown](#html-in-markdown)
   - [Footnotes](#footnotes)
   - [footnote reference](#footnote-reference)
   - [wikilink](#wikilink)
   - [tags](#tags)
   - [latex](#latex)

<!-- md-toc end -->

This is text <!-- a comment --> and more <p style="color:blue;">cccc</p> text.

#### todo

That is so funny! :joy:

I need to highlight these ==very important words==.

H~2~O

X^2^

### My Great Heading

## Emphasis

**Bold text**
*Italic text*
***Bold and italic text***
~~Strikethrough~~

---


## Lists

### Unordered list:

- Item 1
- Sub-item 1
   - Sub-item 2

- Item 2

### Ordered list:
1. First item
2. Second item
   1. Sub-item 1
   2. Sub-item 2

---

## Links

[cc](cds@qq.com)

[Clickable text](https://example.com "csdcsdcs")

[Relative link](./path/to/file.md)

[reff][id]

[id]: https://www.baidu.com "hahah"

[id]: https://www.baidu.com

[foo][]

<localhost:5001/foo>

<https://en.wikipedia.org/wiki>

<foo@bar.example.com>

---

## Images
![Alt text for image](https://via.placeholder.com/150 "cdscs")

![Alt text for image](https://via.placeholder.com/150)

![cc][cc_ref]

[cc_ref]: dog.png "title"

[cc_ref]: dog.png

[1]: <https://en.wikipedia.org/wiki/Hobbit#Lifestyle> (Hobbit lifestyles)

---

## Blockquotes

> This is a blockquote.
> cdcdcscscs
>  > cdcd
>  >      csdc
>>csdc
>  > `cc`

kkkkk

---

## Code

### Inline code:

Use `inline code` like this.

### Code block:

#### fenced code block

```javascript
// Code example in JavaScript
console.log("Hello, world!");
```

```python
# cdcd
print("hellow, world")
```

#### indented code block

    def hello():
        print("Hello, World!")

    hello()

---

## Tables

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Row 1    | Data 1   | Data 2   |
| Row 2    | Data 3   | Data 4   |

---

## Horizontal Rules
You can create a horizontal rule with:

- `kk`
<!-- FIXME -->
- `js` cscs`

---

## Tasks Lists
- [x] Task completed
+ [ ] Task pending
* [ ] Another task pending

---

## Escaping Characters
Escape special characters using a backslash:

\- This renders - as a regular character.

---

## HTML in Markdown

You can include HTML for advanced formatting:
<div style="color:blue;">This text is styled using HTML.</div>

---

## Footnotes
Here is a sentence with a footnote.[^1]

## footnote reference

[^1]: This is the footnote.


## wikilink

Obsidian supports the following link formats:

Wikilink: [[Three laws of motion]]
Markdown: [Three laws of motion](Three%20laws%20of%20motion.md)

## tags

this  is a tag for ex #cc123, and another #d345


## latex

`$$This$$ has $$an odd$$ number of instances of $$.`


foo\
bar


&nbsp;

&amp;

&copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;

&#35; &#1234; &#992; &#0;


---
