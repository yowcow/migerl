-- +migrate Up

create table member (
    id int unsigned auto_increment,
    username varchar(255) not null,
    email varchar(255) not null,
    primary key(id)
) engine=innodb default charset=utf8mb4;

-- +migrate Down

drop table member;
