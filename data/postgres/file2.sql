-- +migrate Up notransaction

CREATE TABLE member_password (
    member_id serial NOT NULL,
    password_hash varchar(255) NOT NULL,
    password_salt varchar(255) NOT NULL,
    PRIMARY KEY (member_id),
    FOREIGN KEY (member_id)
        REFERENCES member (id)
            ON UPDATE CASCADE
            ON DELETE CASCADE
);

-- +migrate Down notransaction

DROP TABLE member_password;
