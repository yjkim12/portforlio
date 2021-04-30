ALTER TABLE pbp DROP FOREIGN KEY FKpitcher_1;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_2;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_3;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_4;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_5;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_6;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_7;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_8;
ALTER TABLE pbp DROP FOREIGN KEY FKfielder_9;

ALTER TABLE pbp DROP FOREIGN KEY FKpitcherID;
ALTER TABLE pbp DROP FOREIGN KEY FKbatterID;

DELETE FROM pbp;


LOAD DATA LOCAL INFILE 'C:/SQL/mlb2018_1.csv'
INTO TABLE pbp
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS
(ID,index_,pitcherID,batterID,@pitch_type,game_date,@release_speed,
@release_pos_x,@release_pos_y,@release_pos_z,@events,description,@zone,
game_type,stand,p_throws,home_team,away_team,type,@hit_location,@bb_type,
balls,strikes,game_year,@pfx_x,@pfx_z,@plate_x,@plate_z,@on_3b,@on_2b,@on_1b,
outs_when_up,inning,inning_topbot,@hc_x,@hc_y,@vx0,@vy0,@vz0,@ax,@ay,@az,
@sz_top,@sz_bot,@hit_distance_sc,@launch_speed,@launch_angle,@effective_speed,
@release_spin_rate,@release_extension,game_pk,
pitcher_1,fielder_2,fielder_3,fielder_4,fielder_5,fielder_6,fielder_7,fielder_8,fielder_9,
@estimated_ba_using_speedangle,@estimated_woba_using_speedangle,@woba_value,@woba_denom,@babip_value,
@iso_value,@launch_speed_angle,at_bat_number,pitch_number,@pitch_name,home_score,away_score,
bat_score,fld_score,post_away_score,post_home_score,post_bat_score,post_fld_score,
@if_fielding_alignment,@of_fielding_alignment,@spin_axis,@delta_home_win_exp,@delta_run_exp,des)
SET 
pitch_type = NULLIF(@pitch_type, ''),
release_speed = NULLIF(@release_speed, ''),
release_pos_x = NULLIF(@release_pos_x, ''),
release_pos_y = NULLIF(@release_pos_y, ''),
release_pos_z = NULLIF(@release_pos_z, ''),
events = NULLIF(@events, ''),
zone = NULLIF(@zone, ''),
hit_location = NULLIF(@hit_location, ''),
bb_type = NULLIF(@bb_type, ''),
pfx_x = NULLIF(@pfx_x, ''),
pfx_z = NULLIF(@pfx_z, ''),
plate_x = NULLIF(@plate_x, ''),
plate_z = NULLIF(@plate_z, ''),
on_3b = NULLIF(@on_3b, ''),
on_2b = NULLIF(@on_2b, ''),
on_1b = NULLIF(@on_1b, ''),
hc_x = NULLIF(@hc_x, ''),
hc_y = NULLIF(@hc_y, ''),
vx0 = NULLIF(@vx0, ''),
vy0 = NULLIF(@vy0, ''),
vz0 = NULLIF(@vz0, ''),
ax = NULLIF(@ax, ''),
ay = NULLIF(@ay, ''),
az = NULLIF(@az, ''),
sz_top = NULLIF(@sz_top, ''),
sz_bot = NULLIF(@sz_bot, ''),
hit_distance_sc = NULLIF(@hit_distance_sc, ''),
launch_speed = NULLIF(@launch_speed, ''),
launch_angle = NULLIF(@launch_angle, ''),
effective_speed = NULLIF(@effective_speed, ''),
release_spin_rate = NULLIF(@release_spin_rate, ''),
release_extension = NULLIF(@release_extension, ''),
estimated_ba_using_speedangle = NULLIF(@estimated_ba_using_speedangle, ''),
estimated_woba_using_speedangle = NULLIF(@estimated_woba_using_speedangle, ''),
woba_value = NULLIF(@woba_value, ''),
woba_denom = NULLIF(@woba_denom, ''),
babip_value = NULLIF(@babip_value, ''),
iso_value = NULLIF(@iso_value, ''),
launch_speed_angle = NULLIF(@launch_speed_angle, ''),
pitch_name = NULLIF(@pitch_name, ''),
if_fielding_alignment = NULLIF(@if_fielding_alignment, ''),
of_fielding_alignment = NULLIF(@of_fielding_alignment, ''),
spin_axis = NULLIF(@spin_axis, ''),
delta_home_win_exp = NULLIF(@delta_home_win_exp, ''),
delta_run_exp = NULLIF(@delta_run_exp, '')
;


