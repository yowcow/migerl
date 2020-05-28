-- +migrate Up

INSERT INTO member (id, name) VALUES
(4, 'member4');

INSERT INTO member_password (member_id, password_hash, password_salt) VALUES
(4, "80cca7e0d00a8e700b383a449aba59c1f4b401b1", "barbar");

-- +migrate Down

DELETE FROM member WHERE id IN (4);
