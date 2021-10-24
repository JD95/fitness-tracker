pragma foreign_keys=off;

begin transaction;

alter table muscles rename to old_muscles;

create table muscle
  ( muscle_id integer primary key
  , name      text 
  , min_rep   int   
  , max_rep   int  
  , min_vol   int  
  , max_vol   int  
  );

insert into muscle
select null, name, min_rep, max_rep, min_vol, max_vol
from old_muscles;

drop table old_muscles;

-- WORKOUT

alter table workout rename to old_workout;

create table workout
  ( workout_id integer primary key
  , name text
  ); 

insert into workout select null, name from old_workout;

drop table old_workout;

-- PRIMARY MUSCLES

alter table primary_muscles rename to old_primary_muscles;

create table primary_muscle
  ( workout int
  , muscle  int 
  , foreign key(workout) references workout(workout_id)
  , foreign key(muscle) references muscle(muscle_id)
  ); 

insert into primary_muscle
select workout_id, muscle_id
from old_primary_muscles
join workout on workout.name = old_primary_muscles.workout
join muscle on muscle.name = old_primary_muscles.muscle;

drop table old_primary_muscles;

-- WORKOUT SET

alter table workout_set rename to old_workout_set;

create table workout_set
  ( workout_set_id integer primary key
  , workout        int 
  , reps           int 
  , date           real 
  , weight         int 
  , intensity      int 
  , foreign key(workout) references workout(workout_id)
  ); 

insert into workout_set
select null, workout.workout_id, reps, old_workout_set.date_of, weight, intensity
from old_workout_set
join workout on workout.name = old_workout_set.name;

drop table old_workout_set;

commit;

pragma foreign_keys=on;
