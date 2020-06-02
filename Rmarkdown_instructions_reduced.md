---
author: "Nick Riches"
name: MiMo
output:
  html_document:
    number_sections: no
    toc: yes
    toc_float:
      collapsed: yes
  word_document:
    toc: yes
  pdf_document:
    toc: yes
---
        




![](logo.png)
        
# Welcome to the Trans-pose translation tool

## What is Trans-pose?

Trans-pose is a tool which uses Google translate to translate sentences. The original sentence is shown above the translated sentences, thereby helping the learner to visualise similarities and differences between the two languages. This process is facilitated by colouring in the words according to their word classes. An example is shown below.

![](translated_text.png)

Here was can see that (a) Spanish does not need a subject (*you*), (b) does not need an auxiliary to form the question (*Did*), and (c) places the adjective (*azul*) after the Noun.

## How do I use it?

Go to the `Enter text` tab above, then enter the text. You can either upload the text from a Word or text file, or paste the text into the box. Then go to the `Check language` tab. It should automatically display the language of the text you have just entered. You need to specify the "to" language in the same text box (using autocomplete). Once you have done this, click on the `Let's explore` tab. This will show you a table with the "from" and "to" above each other, and the word classes coloured in. (NB if you hover above the words, you will see a description of the word classes).

You can isolate particular lines by (a) entering the line number in the `sentence_id` column, (b) searching for particular text in the big search bar at the top (this is *not* the search bar which is used to search inside columns). You can also search for particular word classes by typing `has` and the name of the word class, e.g. `hasaux` will find all the sentences containing auxiliary verbs.

## Changing colours

If you are not happy with the colours of the word classes you may go to the `Colours` tab to change them. This also provides a handy list of the word classes for you.

## A note on accuracy

Neither Google translate, nor the technology which determines the words classes are 100% accurate and reliable. There will inevitably be some odd translations and labellings. This should be borne in mind when using the app.

## A note on word colours

Word colours may only be provided if there is a corresponding grammar. Though there are grammars for most major world languages you may find that you are translating to or from a language for which no grammar exists. If this is the case, then unfortunately the words will not be coloured in.
