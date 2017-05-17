#!/usr/bin/env bash
echo "" > all_txt
cat *.erl >> all_txt
cat */*.erl >> all_txt
cat */*/*.erl >> all_txt
cat */*/*/*.erl >> all_txt
cat */*/*/*/*.erl >> all_txt
cat */*/*/*/*/*.erl >> all_txt
cat */*/*/*/*/*/*.erl >> all_txt
cat */*/*/*/*/*/*/*.erl >> all_txt
