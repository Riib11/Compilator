# Compilator

An abstractly-specified transpiler. The _Compilator_ work-flow is the following:

1. Create a translation-specification that describes how to translate a tagged source-text (looking similar to XML or HTML) into the desired target-text format.
2. Create a source-text that follows the source-format of the previous translation-specification.
3. Run _Compilator_ given the specification and the source-text, yielding a target-text in the specified format.

## Summary

I designed this program to be used for modularizing the type-setting of articles / papers / etc., as I've found it annoyingly manual to maintain various versions of a document. For example if I have a paper written in LaTex, it almost entirely a manual process to copy-paste and then polish the transcript into an HTML format for web-page publishing. Additionally, if I'd like to make any changes to document, I have to make the changes for each format of the document I am supporting. This is obviously sub-optimal, but I haven't found a satisfactory framework that allows the kind of features that _Compilator_ is build to support.

_Compilator_ solves the previously-illustrated problem in this way: I only have to maintain one source-text that is in an abstract XML-like format; to push changes to all formats of the documents is just to change the source-text and then re-_compilating_ it to each target format (e.g. LaTex, HTML, etc.). _Compilator_ also offers a unique ability to maintain a translation-specification for each target format. In this way, I can change the way that a document is typeset in, say, LaTex, without making any changes to the source-text at all.

This is achieved by how _Compilator_ _compilates_ a source-text with a translation-specification:
1. Interpret the source-text as an abstract XML-like tree with tags.
2. Use the translation-specification to recursively translate the tree into the target format; the translation-specification details how each tag (with the allowance of arguments and nesting) should be translated into the target format.
3. The translated tree is written in the target format in some specified outline / order.
