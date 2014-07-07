# Weblocks CMS Mailings plugin

A plugin for quick mailing in your Weblocks application.

Its feature are: 

* Mass mailings with filtered targets 

* Single emails 

* Preview all emails before sending 

* Spellcheck your emails with yandex speller


Currently supports sendmail transport.

## Requirements

Requires Weblocks, tested with 0.10.19 and Weblocks CMS, tested with 0.4.8

## Integrating with your application

First important thing is that you should have emails collected somewhere.
You should have "email model" - class for emails. Each "email model" instance should have one email.
For example class `'user` with slot `'email`. It should work with plugin.
Model means Weblocks Stores model.

To say "Weblocks CMS Mailings" you have such model you should do something like

```lisp 
(defparameter weblocks-cms-mailings:*email-model* 'weblocks-cms::user) 
```

And to say "Weblocks CMS Mailings" which value to use for each model instance, you should define `weblocks-cms-mailings:email-model-email-address`  which should return email.

Other important thing is email from address, it can be set By

```lisp 
(defparameter weblocks-cms-mailings:*default-email-from* +email-from+)
```

## Options 

Apart from integration options there is one for spellcheck - `weblocks-cms-mailings:*spellcheck-enabled-p*`.
Currently it checks spelling during mass mailings. Warning, it can take a long time for it.
