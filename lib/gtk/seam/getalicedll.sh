#!/bin/sh

echo $(alice-config --alicebindir | sed 's#/bin$##g')/lib/seam/alice.dll
