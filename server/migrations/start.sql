create table muscles
  ( name    text
  , min_rep int
  , max_rep int
  , min_vol int
  , max_vol int
  );

create table primary_muscles
  ( workout text
  , muscle  text
  ); 

create table workout
  ( name text
  ); 

create table workout_set
  ( name      text
  , reps      int
  , date_of   real
  , weight    int
  , intensity int
  ); 
