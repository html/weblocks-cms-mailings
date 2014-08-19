# Weblocks CMS Mailings plugin

A plugin for quick mailing in your Weblocks application.

Its feature are: 

* Mass mailings with filtered targets 

* Single emails 

* Preview all emails before sending, see emails already sent.

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

It is required to start `cl-cron` for messages sending. 
Also, you should call `cl-cron:make-cron-job` with `'weblocks-cms-mailings:send-messages` as a parameter

Also remember that only `weblocks-cms::message` objects with status `ready-to-send` would be sent.
You should set status automatically or manually. 
Default status for mailings is `created`, so you can preview emails but they will be sent via `cl-cron` job only after you set right status.

When you need to remove some records from email targets filters, for example users without emails should not be there, you can use `weblocks-cms-mailings:email-model-display-in-grid-p` method.
See the source for example.

## Options 

Apart from integration options there is one for spellcheck - `weblocks-cms-mailings:*spellcheck-enabled-p*`.
Currently it checks spelling during mass mailings. Warning, it can take a long time for it.
