#!/bin/sh

  sed "s/>>/~>>/g" |
  sed "s/%</<</g" |
  sed "s/>%/>>/g" |
  sed "s/%:xmllist</<:xmllist</g" |
  sed "s/(\*zap\*/\n(\*zap\*\n/g" | 
  sed "s/\*zap\*)/\n\*zap\*)\nPLOPOPO/g" | 
  sed '/(\*zap\*/,/\*zap\*)/d'| 
  sed ':a;N;$!ba;s/\nPLOPOPO//g' | 
  sed 's/\*wiki\*)/<<code language="ocaml" |/g' | 
  sed 's/(\*wiki\*/>>/g'


