-- +migrate Up

create table member_passwd (
    member_id int unsigned,
    passwd_hash varchar(255) not null,
    passwd_salt varchar(255) not null,
    primary key (member_id),
    foreign key member_id_fkey (member_id)
        references member (id)
        on delete cascade
        on update cascade
) engine=innodb default charset=utf8mb4;

-- +migrate Down

drop table member_passwd;
