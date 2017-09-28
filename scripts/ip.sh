#!/bin/bash

if [ $(pgrep -x openvpn) ]; then
  dig TXT +short o-o.myaddr.l.google.com @ns1.google.com | awk -F'"' '{ print $2}'
else
  ip addr show wlp3s0 | grep -w inet | awk '{print $2}' | sed 's/\/.*//'
fi
