-- +migrate Up notransaction

CREATE TABLE member (
    id serial not null,
    name varchar(255) NOT NULL,
    PRIMARY KEY(id)
);

-- +migrate Down notransaction

DROP TABLE member;
