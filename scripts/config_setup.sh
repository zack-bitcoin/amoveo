#!/bin/bash

if [[ ! -e ./config/sys.config.tmpl ]]; then
    cp ./config/sys.config.tmpl.default ./config/sys.config.tmpl
fi
