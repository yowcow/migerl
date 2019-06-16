-- +migrate Up

INSERT INTO member (id, name) VALUES
(1, 'member1'),
(2, 'member2'),
(3, 'member3');

INSERT INTO member_password (member_id, password_hash, password_salt) VALUES
(1, "cd303f921f510ff6b5daa1d6f0747e2c7be978a6", "fugafuga"),
(2, "295191167f8092e7f7221d2e0416d326ab9131a6", "foofoo"),
(2, "80cca7e0d00a8e700b383a449aba59c1f4b401b1", "barbar");

-- +migrate Down

DELETE FROM member WHERE id IN (1, 2, 3);
