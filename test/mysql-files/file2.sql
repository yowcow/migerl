-- +migrate Up notransaction

CREATE TABLE member_password (
    member_id int(11) unsigned NOT NULL,
    password_hash varchar(255) NOT NULL,
    password_salt varchar(255) NOT NULL,
    PRIMARY KEY(member_id),
    FOREIGN KEY `member_fk1` (member_id)
        REFERENCES member (id)
            ON UPDATE CASCADE
            ON DELETE CASCADE
) ENGINE=InnoDB default charset=utf8;

-- +migrate Down notransaction

DROP TABLE member_password;
