-- +migrate Up

create unique index email_uniq on member (email);

-- +migrate Down

alter table member drop index email_uniq;
