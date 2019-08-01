-- +migrate Up notransaction

CREATE TABLE member (
    id int(11) unsigned NOT NULL AUTO_INCREMENT,
    name varchar(255) NOT NULL,
    PRIMARY KEY(id)
) ENGINE=InnoDB default charset=utf8;

-- +migrate Down notransaction

DROP TABLE member;
