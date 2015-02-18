#!/bin/sh
scriptdir=`dirname $0`

cd ~/Dropbox/berkeley/Programming-resources/stanford-ner-2014-10-26/

java -mx1500m -cp stanford-ner.jar edu.stanford.nlp.ie.NERServer -loadClassifier classifiers/english.all.3class.distsim.crf.ser.gz -port 8080 -outputFormat inlineXML