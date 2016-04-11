#!/usr/bin/env bash

## 上传文件
curl --verbose --header "restrict-access:true" -form file=/etc/ejabberd/ejabberd.yml http://innodealing-dev:8080/upload/

## 访问文件

## 登录