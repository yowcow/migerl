-- +migrate Up

insert into member_passwd set
    member_id = 1,
    passwd_hash = 'hogehoge1',
    passwd_salt = 'fugafuga1';

insert into member_passwd set
    member_id = 2,
    passwd_hash = 'hogehoge2',
    passwd_salt = 'fugafuga2';

insert into member_passwd set
    member_id = 3,
    passwd_hash = 'hogehoge3',
    passwd_salt = 'fugafuga3';

insert into member_passwd set
    member_id = 4,
    passwd_hash = 'hogehoge4',
    passwd_salt = 'fugafuga4';

-- +migrate Down

delete from member_passwd where member_id = 1;
delete from member_passwd where member_id = 2;
delete from member_passwd where member_id = 3;
delete from member_passwd where member_id = 4;
delete from member_passwd where member_id = 5;
