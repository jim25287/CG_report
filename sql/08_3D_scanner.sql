SELECT
  member_id AS client_id,
  data->>'Body Height' AS body_height,
  data->>'Rise Height' AS rise_height,
  data->>'Scye Depth A' AS scye_depth_a,
  data->>'Halter Length' AS halter_length,
  data->>'Vertical Trunk' AS vertical_trunk,
  data->>'Left Arm Length' AS left_arm_length,
  data->>'Ref Chest Circum' AS ref_chest_circum,
  data->>'Right Arm Length' AS right_arm_length,
  data->>'Left Scye Depth B' AS left_scye_depth_b,
  data->>'Pants Rise Length' AS pants_rise_length,
  data->>'Total Rise Length' AS total_rise_length,
  data->>'Left Inseam Length' AS left_inseam_length,
  data->>'Left Sleeve Length' AS left_sleeve_length,
  data->>'Ref Low Hip Circum' AS ref_low_hip_circum,
  data->>'Right Scye Depth B' AS right_scye_depth_b,
  data->>'Apex to Apex Length' AS apex_to_apex_length,
  data->>'Belly Circumference' AS belly_circumference,
  data->>'Bust sub Under Bust' AS bust_sub_under_bust,
  data->>'Chest Circumference' AS chest_circumference,
  data->>'Ref High Hip Circum' AS ref_high_hip_circum,
  data->>'Right Inseam Length' AS right_inseam_length,
  data->>'Right Sleeve Length' AS right_sleeve_length,
  data->>'Collar Circumference' AS collar_circumference,
  data->>'F Bust Circumference' AS f_bust_circumference,
  data->>'Left Shoulder Length' AS left_shoulder_length,
  data->>'Ref Left Knee Circum' AS ref_left_knee_circum,
  data->>'Ref Neck Base Circum' AS ref_neck_base_circum,
  data->>'Left Under Arm Length' AS left_under_arm_length,
  data->>'Low Hip Circumference' AS low_hip_circumference,
  data->>'Pants Circumference A' AS pants_circumference_a,
  data->>'Ref Left Ankle Circum' AS ref_left_ankle_circum,
  data->>'Ref Left Elbow Circum' AS ref_left_elbow_circum,
  data->>'Ref Left Wrist Circum' AS ref_left_wrist_circum,
  data->>'Ref Right Knee Circum' AS ref_right_knee_circum,
  data->>'Right High Hip Height' AS right_high_hip_height,
  data->>'Right Shoulder Length' AS right_shoulder_length,
  data->>'Back Pants Rise Height' AS back_pants_rise_height,
  data->>'Back Pants Rise Length' AS back_pants_rise_length,
  data->>'High Hip Circumference' AS high_hip_circumference,
  data->>'Ref Right Ankle Circum' AS ref_right_ankle_circum,
  data->>'Ref Right Elbow Circum' AS ref_right_elbow_circum,
  data->>'Ref Right Wrist Circum' AS ref_right_wrist_circum,
  data->>'Right Under Arm Length' AS right_under_arm_length,
  data->>'Underpec Circumference' AS underpec_circumference,
  data->>'Front Pants Rise Height' AS front_pants_rise_height,
  data->>'Front Pants Rise Length' AS front_pants_rise_length,
  data->>'Left Calf Circumference' AS left_calf_circumference,
  data->>'Left Knee Circumference' AS left_knee_circumference,
  data->>'Left NSP to Apex Length' AS left_nsp_to_apex_length,
  data->>'Neck Base Circumference' AS neck_base_circumference,
  data->>'Overall Circumference A' AS overall_circumference_a,
  data->>'Overall Circumference B' AS overall_circumference_b,
  data->>'Ref Narrow Waist Circum' AS ref_narrow_waist_circum,
  data->>'Left Ankle Circumference' AS left_ankle_circumference,
  data->>'Left Ankle Inseam Length' AS left_ankle_inseam_length,
  data->>'Left Elbow Circumference' AS left_elbow_circumference,
  data->>'Left Thigh Circumference' AS left_thigh_circumference,
  data->>'Left Wrist Circumference' AS left_wrist_circumference,
  data->>'Low Hip sub Narrow_Waist' AS low_hip_sub_narrow_waist,
  data->>'Right Calf Circumference' AS right_calf_circumference,
  data->>'Right Knee Circumference' AS right_knee_circumference,
  data->>'Right NSP to Apex Length' AS right_nsp_to_apex_length,
  data->>'Armpit Around Neck Length' AS armpit_around_neck_length,
  data->>'Back Cross Shoulder Width' AS back_cross_shoulder_width,
  data->>'Left Pants Outseam Length' AS left_pants_outseam_length,
  data->>'Left Shoulder Drop Degree' AS left_shoulder_drop_degree,
  data->>'Left Shoulder Drop Height' AS left_shoulder_drop_height,
  data->>'Right Ankle Circumference' AS right_ankle_circumference,
  data->>'Right Ankle Inseam Length' AS right_ankle_inseam_length,
  data->>'Right Elbow Circumference' AS right_elbow_circumference,
  data->>'Right Thigh Circumference' AS right_thigh_circumference,
  data->>'Right Wrist Circumference' AS right_wrist_circumference,
  data->>'Upper Back Width Length A' AS upper_back_width_length_a,
  data->>'Upper Back Width Length B' AS upper_back_width_length_b,
  data->>'Upper Back Width Length C' AS upper_back_width_length_c,
  data->>'F Under Bust Circumference' AS f_under_bust_circumference,
  data->>'Front Cross Shoulder Width' AS front_cross_shoulder_width,
  data->>'Left Forearm Circumference' AS left_forearm_circumference,
  data->>'Narrow Waist Circumference' AS narrow_waist_circumference,
  data->>'Neck Shoulder Points Width' AS neck_shoulder_points_width,
  data->>'Right Pants Outseam Length' AS right_pants_outseam_length,
  data->>'Right Shoulder Drop Degree' AS right_shoulder_drop_degree,
  data->>'Right Shoulder Drop Height' AS right_shoulder_drop_height,
  data->>'Side Neck Base Diff Height' AS side_neck_base_diff_height,
  data->>'Left Arm Hole Circumference' AS left_arm_hole_circumference,
  data->>'Right Forearm Circumference' AS right_forearm_circumference,
  data->>'Left Arm Length to Back Neck' AS left_arm_length_to_back_neck,
  data->>'Left Mid Thigh Circumference' AS left_mid_thigh_circumference,
  data->>'Left Upper Arm Circumference' AS left_upper_arm_circumference,
  data->>'Right Arm Hole Circumference' AS right_arm_hole_circumference,
  data->>'Thinnest Waist Circumference' AS thinnest_waist_circumference,
  data->>'Left NSP to Back Belly Length' AS left_nsp_to_back_belly_length,
  data->>'Left NSP to Back Chest Length' AS left_nsp_to_back_chest_length,
  data->>'Left Side Narrow_Waist Length' AS left_side_narrow_waist_length,
  data->>'Right Arm Length to Back Neck' AS right_arm_length_to_back_neck,
  data->>'Right Mid Thigh Circumference' AS right_mid_thigh_circumference,
  data->>'Right Upper Arm Circumference' AS right_upper_arm_circumference,
  data->>'Back Left NSP to Crotch Length' AS back_left_nsp_to_crotch_length,
  data->>'Center Back Neck to NSP Length' AS center_back_neck_to_nsp_length,
  data->>'Left Mid Thigh Circumference B' AS left_mid_thigh_circumference_b,
  data->>'Left NSP to Front Belly Length' AS left_nsp_to_front_belly_length,
  data->>'Left NSP to Front Chest Length' AS left_nsp_to_front_chest_length,
  data->>'Right NSP to Back Belly Length' AS right_nsp_to_back_belly_length,
  data->>'Right NSP to Back Chest Length' AS right_nsp_to_back_chest_length,
  data->>'Right Side Narrow_Waist Length' AS right_side_narrow_waist_length,
  data->>'Back Right NSP to Crotch Length' AS back_right_nsp_to_crotch_length,
  data->>'Front Left NSP to Crotch Length' AS front_left_nsp_to_crotch_length,
  data->>'Left NSP to Back Low Hip Length' AS left_nsp_to_back_low_hip_length,
  data->>'Left Pants Ankle Outseam Length' AS left_pants_ankle_outseam_length,
  data->>'NSP to Center Front Neck Length' AS nsp_to_center_front_neck_length,
  data->>'Right Mid Thigh Circumference B' AS right_mid_thigh_circumference_b,
  data->>'Right NSP to Front Belly Length' AS right_nsp_to_front_belly_length,
  data->>'Right NSP to Front Chest Length' AS right_nsp_to_front_chest_length,
  data->>'Front Right NSP to Crotch Length' AS front_right_nsp_to_crotch_length,
  data->>'Left NSP to Back Underpec Length' AS left_nsp_to_back_underpec_length,
  data->>'Left NSP to Front Low Hip Length' AS left_nsp_to_front_low_hip_length,
  data->>'Left Narrow_Waist Outseam Length' AS left_narrow_waist_outseam_length,
  data->>'Right NSP to Back Low Hip Length' AS right_nsp_to_back_low_hip_length,
  data->>'Right Pants Ankle Outseam Length' AS right_pants_ankle_outseam_length,
  data->>'Upper Front Chest Width Length A' AS upper_front_chest_width_length_a,
  data->>'Upper Front Chest Width Length B' AS upper_front_chest_width_length_b,
  data->>'Upper Front Chest Width Length C' AS upper_front_chest_width_length_c,
  data->>'Center Back Neck to Crotch Length' AS center_back_neck_to_crotch_length,
  data->>'Center Front Neck to Belly Length' AS center_front_neck_to_belly_length,
  data->>'Center Front Neck to Chest Length' AS center_front_neck_to_chest_length,
  data->>'Left NSP to Front Underpec Length' AS left_nsp_to_front_underpec_length,
  data->>'Right NSP to Back Underpec Length' AS right_nsp_to_back_underpec_length,
  data->>'Right NSP to Front Low Hip Length' AS right_nsp_to_front_low_hip_length,
  data->>'Right Narrow_Waist Outseam Length' AS right_narrow_waist_outseam_length,
  data->>'Center Front Neck to Crotch Length' AS center_front_neck_to_crotch_length,
  data->>'Ref Center Back Neck to Hip Length' AS ref_center_back_neck_to_hip_length,
  data->>'Right NSP to Front Underpec Length' AS right_nsp_to_front_underpec_length,
  data->>'Ref Center Front Neck to Hip Length' AS ref_center_front_neck_to_hip_length,
  data->>'Below Shoulder Overall Circumference' AS below_shoulder_overall_circumference,
  data->>'Left NSP to Back Narrow_Waist Length' AS left_nsp_to_back_narrow_waist_length,
  data->>'Upper Front Chest Around Neck Length' AS upper_front_chest_around_neck_length,
  data->>'Left NSP to Front Narrow_Waist Length' AS left_nsp_to_front_narrow_waist_length,
  data->>'Left Side Narrow_Waist to Knee Length' AS left_side_narrow_waist_to_knee_length,
  data->>'Right NSP to Back Narrow_Waist Length' AS right_nsp_to_back_narrow_waist_length,
  data->>'Left NSP to Back Thinnest_Waist Length' AS left_nsp_to_back_thinnest_waist_length,
  data->>'Left Side Narrow_Waist to Ankle Length' AS left_side_narrow_waist_to_ankle_length,
  data->>'Right NSP to Front Narrow_Waist Length' AS right_nsp_to_front_narrow_waist_length,
  data->>'Right Side Narrow_Waist to Knee Length' AS right_side_narrow_waist_to_knee_length,
  data->>'Center Back Neck to Narrow_Waist Length' AS center_back_neck_to_narrow_waist_length,
  data->>'Center Back Neck to Upper Back Length A' AS center_back_neck_to_upper_back_length_a,
  data->>'Center Back Neck to Upper Back Length B' AS center_back_neck_to_upper_back_length_b,
  data->>'Center Back Neck to Upper Back Length C' AS center_back_neck_to_upper_back_length_c,
  data->>'Right NSP to Back Thinnest_Waist Length' AS right_nsp_to_back_thinnest_waist_length,
  data->>'Right Side Narrow_Waist to Ankle Length' AS right_side_narrow_waist_to_ankle_length,
  data->>'Center Front Neck to Narrow_Waist Length' AS center_front_neck_to_narrow_waist_length,
  data->>'Left Side Narrow_Waist to Low Hip Length' AS left_side_narrow_waist_to_low_hip_length,
  data->>'Center Back Neck to Thinnest_Waist Length' AS center_back_neck_to_thinnest_waist_length,
  data->>'Left Side Narrow_Waist to High Hip Length' AS left_side_narrow_waist_to_high_hip_length,
  data->>'Right Side Narrow_Waist to Low Hip Length' AS right_side_narrow_waist_to_low_hip_length,
  data->>'Center Front Neck to Thinnest_Waist Length' AS center_front_neck_to_thinnest_waist_length,
  data->>'Right Side Narrow_Waist to High Hip Length' AS right_side_narrow_waist_to_high_hip_length,
  data->>'Narrow_Waist Horizontal Pants Circumference' AS narrow_waist_horizontal_pants_circumference,
  data->>'Center Front Neck to Upper Front Chest Length A' AS center_front_neck_to_upper_front_chest_length_a,
  data->>'Center Front Neck to Upper Front Chest Length B' AS center_front_neck_to_upper_front_chest_length_b,
  data->>'Center Front Neck to Upper Front Chest Length C' AS center_front_neck_to_upper_front_chest_length_c
FROM measurements
WHERE equip_brand = 'TG3D'
  AND member_type = 'Client'

