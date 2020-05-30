-- +migrate Up

insert into member (username, email) values
("yowcow", "yowcow@x28.co"),
("yowcow", "yowcow@x19.dev"),
("yowcow", "yowcow@cpan.org"),
("yowcow", "yowcow@gmail.com")
;

-- +migrate Down

delete from member
where
    email in (
        "yowcow@x28.co",
        "yowcow@x19.dev",
        "yowcow@cpan.org",
        "yowcow@gmail.com"
    )
