--  Marketing Analytics - Assignment 3


use ma_charity_full;

 # Perimeters
 
 # drop  table Ass3_cont_train;
 drop table Ass3_contacts_train;
 CREATE TABLE Ass3_contacts_train (
  `contact_id` int(10) unsigned ,
  PRIMARY KEY (`contact_id`)
) ENGINE=MyISAM;
 
 insert into Ass3_contacts_train
 select distinct contact_id 
  from acts
  where year(act_date) = 2016;
  

  CREATE TABLE Ass3_contacts_test (
  `contact_id` int(10) unsigned ,
  PRIMARY KEY (`contact_id`)
) ENGINE=MyISAM;

insert into Ass3_contacts_test
select distinct contact_id
  from acts
  where year(act_date) >= 1990 and year(act_date) <= 2017;
 
 
 
######################### previous code ###############################


drop table Ass3_train_my_dataset;
CREATE TABLE Ass3_train_my_dataset (
  `contact_id` int(10) unsigned ,
  `recency` float,
  `frequency` integer(5) DEFAULT NULL,
  `avgamount` float DEFAULT NULL,
  `firstdonation` float DEFAULT NULL,
  `loyal` boolean,
  PRIMARY KEY (`contact_id`)
) ENGINE=MyISAM;

insert into Ass3_train_my_dataset
select assignment2.contact_id as contact_id , b.recency, b.frequency, b.avgamount, b.firstdonation, b.loyal
 from ass3_contacts_train assignment2
 left
 join 
  ( SELECT a.contact_id  as 'contact_id',
			DATEDIFF(20160101, MAX(a.act_date)) / 365 AS 'recency',
            COUNT(a.amount) AS 'frequency',
            AVG(a.amount) AS 'avgamount',
			DATEDIFF(20160101, MIN(a.act_date)) / 365 AS 'firstdonation',
            IF(c.counter IS NULL, 0, 1) AS 'loyal'
         FROM acts a
         LEFT JOIN (SELECT contact_id, COUNT(amount) AS counter
                    FROM Acts
                    WHERE (act_date >= 20150101) AND
                          (act_date <  20160101) AND
                          (act_type_id = 'DO')
                    GROUP BY contact_id) AS c
         ON c.contact_id = a.contact_id
         WHERE (act_type_id = 'DO') AND (act_date < 20160101)
         GROUP BY 1
  ) b
on b.contact_id = assignment2.contact_id;
-- prepei na dw poioi exoun NULL kai pou? 

select count(*) from Ass3_train_my_dataset;  # 236.885


-- My dataset_PA
drop table ass3_train_my_dataset_PA;
CREATE TABLE ass3_train_my_dataset_PA (
  `contact_id` int(10) unsigned ,
  `PA_recency` float,
  `PA_frequency` integer(5) DEFAULT NULL,
  `PA_avgamount` float DEFAULT NULL,
  `PA_firstdonation` float DEFAULT NULL,
  `PA_loyal` boolean,
  PRIMARY KEY (`contact_id`)
) ENGINE=MyISAM;


insert into ass3_train_my_dataset_PA
select assignment2.contact_id as contact_id,
		b.recency as PA_recency, 
        b.frequency as PA_frequency, 
        b.avgamount as PA_avgamount, 
        b.firstdonation as PA_firstdonation, 
        b.loyal as PA_loyal
 from ass3_contacts_train assignment2
 left
 join 
  ( SELECT a.contact_id  as 'contact_id',
			DATEDIFF(20160101, MAX(a.act_date)) / 365 AS 'recency',
            COUNT(a.amount) AS 'frequency',
            AVG(a.amount) AS 'avgamount',
			DATEDIFF(20160101, MIN(a.act_date)) / 365 AS 'firstdonation',
            IF(c.counter IS NULL, 0, 1) AS 'loyal'
         FROM acts a
         LEFT JOIN (SELECT contact_id, COUNT(amount) AS counter
                    FROM Acts
                    WHERE (act_date >= 20150101) AND
                          (act_date <  20160101) AND
                          (act_type_id = 'PA')
                    GROUP BY contact_id) AS c
         ON c.contact_id = a.contact_id
         WHERE (act_type_id = 'PA') AND (act_date < 20160101)
         GROUP BY 1
  ) b
on b.contact_id = assignment2.contact_id;

select count(*) from ass3_train_my_dataset_PA;

-- My_dataset_PA_DO
drop table Ass3_train_my_dataset_PA_DO;
CREATE TABLE Ass3_train_my_dataset_PA_DO (
  `contact_id` int(10) unsigned ,
  `DO_recency` float,
  `DO_frequency` integer(5) DEFAULT NULL,
  `DO_avgamount` float DEFAULT NULL,
  `DO_firstdonation` float DEFAULT NULL,
  `DO_loyal` boolean,
  `PA_recency` float,
  `PA_frequency` integer(5) DEFAULT NULL,
  `PA_avgamount` float DEFAULT NULL,
  `PA_firstdonation` float DEFAULT NULL,
  `PA_loyal` boolean,
  `PA_active_flag` boolean,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;


 
insert into Ass3_train_my_dataset_PA_DO
select my_dataset.contact_id as contact_id,
       my_dataset.recency as DO_recency, 
	   my_dataset.frequency as DO_frequency, 
       my_dataset.avgamount as DO_avgamount, 
       my_dataset.firstdonation as DO_firstdonation, 
       my_dataset.loyal as PA_loyal,
       my_dataset_PA.PA_recency as PA_recency, 
	   my_dataset_PA.PA_frequency as PA_frequency, 
       my_dataset_PA.PA_avgamount as PA_avgamount, 
       my_dataset_PA.PA_firstdonation as PA_firstdonation, 
       my_dataset_PA.PA_loyal as PA_loyal,
       IF(acts_group.contact_id IS NULL, 0, 1) as PA_active_flag
 from ass3_train_my_dataset       my_dataset
 left
 join ass3_train_my_dataset_PA    my_dataset_PA
   on my_dataset_PA.contact_id = my_dataset.contact_id
 left 
 join 
 (
 select contact_id             -- there were duplicates, some contacts more than two PA dontations on 06-2018
   from acts
  where acts.act_type_id = 'PA'
  and year(acts.act_date) = 2015
  and month(acts.act_date) = 12
group by contact_id
 ) as acts_group
 on acts_group.contact_id = my_dataset.contact_id
 ;

##################################### 
 drop table Ass3_train_Contact_Sol_acc;
-- Solitarities accuracy
CREATE TABLE Ass3_train_Contact_Sol_acc (
  `contact_id` int(10) unsigned ,
  `Solicitation_accuracy` float,
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;


 -- succesful messages
 insert into Ass3_train_Contact_Sol_acc
 select z.contact_id,
        if(y.contact_id is NULL, 0, y.successful_sol/z.camp_sol) as sol_accuracy
  from (
	 select contact_id, count(campaign_id) as camp_sol     -- sinolika solicitations pou stalthikan
	  from actions
	  group by contact_id) as z
     left 
     join (
	 select contact_id, count(*) as successful_sol         -- gia tis campaigns pou esteilan, posoi esteilan (px soy esteilan gia 5 campaigns kai plirwses se 2, 2/5)
	  from (											   -- den metraei poses fores sou esteile i poses fores plirwses se kathe campaign
		 select a.contact_id as contact_id, a.campaign_id as campaign_id
		  from (
		        select contact_id, campaign_id                    -- contact_id, campaign_id (actions)
		        from actions
                where year(action_date) <= 2015
		        group by contact_id, campaign_id
		       )as a
		  inner
		  join (
		        select contact_id, campaign_id                   -- contact_id, campaign_id (acts)
		        from acts
                where year(act_date) <= 2015
		        group by contact_id, campaign_id
		       )as b
		  on a.contact_id = b.contact_id
		  and a.campaign_id = b.campaign_id
		  ) c
		  group by contact_id ) as y
          on y.contact_id = z.contact_id
  ;

drop table ass3_train_my_dataset_PA_DO_Sol;
-- Add all the features and NULL values
CREATE TABLE ass3_train_my_dataset_PA_DO_Sol (
  `contact_id` int(10) unsigned ,
  `DO_recency` float,
  `DO_frequency` integer(5) DEFAULT NULL,
  `DO_avgamount` float DEFAULT NULL,
  `DO_firstdonation` float DEFAULT NULL,
  `DO_loyal` boolean,
  `PA_recency` float,
  `PA_frequency` integer(5) DEFAULT NULL,
  `PA_avgamount` float DEFAULT NULL,
  `PA_firstdonation` float DEFAULT NULL,
  `PA_loyal` boolean,
  `PA_active_flag` boolean,
  `Solicitation_Accuracy` float(7,4),
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;



insert into ass3_train_my_dataset_PA_DO_Sol
select my_dataset_PA_DO.contact_id, 
       ifnull(my_dataset_PA_DO.DO_recency, 40),   # na to koitaksw auto to random variable
       ifnull(my_dataset_PA_DO.DO_frequency, 0),
       ifnull(my_dataset_PA_DO.DO_avgamount, 0),
       ifnull(my_dataset_PA_DO.DO_firstdonation, 40),
       ifnull(my_dataset_PA_DO.DO_loyal, 0) ,
       ifnull(my_dataset_PA_DO.PA_recency, 40),   # auto den einai swsto
       ifnull(my_dataset_PA_DO.PA_frequency, 0),
       ifnull(my_dataset_PA_DO.PA_avgamount, 0),
       ifnull(my_dataset_PA_DO.PA_firstdonation, 40),   # na to ksanakoitaksw auto
       ifnull(my_dataset_PA_DO.PA_loyal, 0),
	   ifnull(my_dataset_PA_DO.PA_active_flag, 0),
       if(Contact_Sol_acc.contact_id  is NULL, 0, Solicitation_accuracy) as Solicitation_Accuracy
  from ass3_train_my_dataset_PA_DO  my_dataset_PA_DO
  left 
  join ass3_train_Contact_Sol_acc  Contact_Sol_acc
    on Contact_Sol_acc.contact_id = my_dataset_PA_DO.contact_id
   order by my_dataset_PA_DO.contact_id;


-- Add to the previous data, the labels of the amounts we want to predict

drop table ass3_train_my_dataset_PA_DO_Sol_label;

CREATE TABLE ass3_train_my_dataset_PA_DO_Sol_label (
  `contact_id` int(10) unsigned ,
  `DO_recency` float,
  `DO_frequency` integer(5) DEFAULT NULL,
  `DO_avgamount` float DEFAULT NULL,
  `DO_firstdonation` float DEFAULT NULL,
  `DO_loyal` boolean,
  `PA_recency` float,
  `PA_frequency` integer(5) DEFAULT NULL,
  `PA_avgamount` float DEFAULT NULL,
  `PA_firstdonation` float DEFAULT NULL,
  `PA_loyal` boolean,
  `PA_active_flag` boolean,
  `Solicitation_Accuracy` float(7,4),
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;


insert into ass3_train_my_dataset_PA_DO_Sol_label
select 
      my_dataset_PA_DO_Sol.contact_id                                   ,
      my_dataset_PA_DO_Sol.DO_recency                                   ,
      my_dataset_PA_DO_Sol.DO_frequency                                 ,
      my_dataset_PA_DO_Sol.DO_avgamount                                 ,
      my_dataset_PA_DO_Sol.DO_firstdonation                             ,
      my_dataset_PA_DO_Sol.DO_loyal                                     ,
      my_dataset_PA_DO_Sol.PA_recency                                   ,
      my_dataset_PA_DO_Sol.PA_frequency                                 ,
      my_dataset_PA_DO_Sol.PA_avgamount                                 ,
      my_dataset_PA_DO_Sol.PA_firstdonation                             ,
      my_dataset_PA_DO_Sol.PA_loyal                                     ,
      my_dataset_PA_DO_Sol.PA_active_flag                               ,
      my_dataset_PA_DO_Sol.Solicitation_Accuracy           
 from ass3_train_my_dataset_PA_DO_Sol     my_dataset_PA_DO_Sol
inner 
 join ass3_contacts_train              assignment2
   on assignment2.contact_id = my_dataset_PA_DO_Sol.contact_id;

select count(*) from ass3_train_my_dataset_PA_DO_Sol_label;  # 236885
select * from ass3_train_my_dataset_PA_DO_Sol_label;
--------------------- Round 2 -------------------------
################# Den mporw na to xrisimopoiisw stin periptwsi mas #####
 /* CREATE TABLE C189_number_sol (
  `contact_id` int(10) unsigned ,
  `sol_count_C189` integer(5) DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_C189_number_sol
select my_dataset_pa_do_sol_label.contact_id  as contact_id, 
       count(*)  as sol_count_C189
  from ass3_train_my_dataset_pa_do_sol_label  my_dataset_pa_do_sol_label
  inner
  join actions  actions
    on actions.contact_id = my_dataset_pa_do_sol_label.contact_id
   and actions.campaign_id = "C189" 
group by my_dataset_pa_do_sol_label.contact_id;   */

drop table ass3_train_sol_accuracy_measures;
CREATE TABLE ass3_train_sol_accuracy_measures (   -- exei noima na spasei se DO kai PA?
  `contact_id` int(10) unsigned ,
  `Sol_Accuracy1` float(7,4),
  `Sol_Accuracy2` float(7,4),
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_sol_accuracy_measures
select assignment2.contact_id as contact_id,
       x.sol_accuracy1       as sol_accuracy1,
       x.sol_accuracy2        as sol_accuracy2
  from ass3_contacts_train  assignment2
  left 
  join (
select z.contact_id,
        if(y.contact_id is NULL, 0, y.successful_sol/z.camp_sol) as sol_accuracy1,
         if(y.contact_id is NULL, 0, y.successful_sol/z.dist_camp_sol) as sol_accuracy2
  from (
	 select contact_id, 
			count(campaign_id) as camp_sol,    -- sinolika solicitations pou stalthikan (gia ena campaign mporei na stalthikan 3)
            count(distinct campaign_id) as dist_camp_sol   -- metraw mia fora to kathe campaign
	  from actions
      where year(action_date) <=2015
	  group by contact_id) as z
     left 
     join (
	 select contact_id, count(*) as successful_sol      -- succesful solicitations, se poses campaigns apantise (eite etseile 10 dwrees eite 1, metraw 1 successful sol)
	  from (											   
		 select a.contact_id as contact_id, a.campaign_id as campaign_id  -- succesful campaigns
		  from (
		        select contact_id, campaign_id                    -- contact_id, campaign_id (actions)
		        from actions
                where year(action_date) <= 2015
		        group by contact_id, campaign_id
		       )as a
		  inner
		  join (
		        select contact_id, campaign_id                   -- contact_id, campaign_id (acts)
		        from acts
                where year(act_date) <= 2015
		        group by contact_id, campaign_id
		       )as b
		  on a.contact_id = b.contact_id
		  and a.campaign_id = b.campaign_id
		  ) c
		  group by contact_id ) as y
          on y.contact_id = z.contact_id ) x
          on x.contact_id = assignment2.contact_id
  ; 



drop table ass3_train_Solicitation_Efficiency;
 CREATE TABLE  ass3_train_Solicitation_Efficiency(       -- ana epitiximena campaigns posa solicitations dextike
  `contact_id` int(10) unsigned ,
  `Averga_number_sol` integer(5) DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;


insert into ass3_train_Solicitation_Efficiency
select assignment2.contact_id  as contact_id,
       x.averga_number_sol     as averga_number_sol
   from ass3_contacts_train   assignment2
   left
   join
   (
select d.contact_id, avg(d.numbofsol) as averga_number_sol
  from (
select actions.contact_id, actions.campaign_id, count(*) as numbofsol   -- ana epitiximena campaigns posa solicitations dextike
  from actions actions
  inner
  join
  (
### etsi vriskw tis successful campaigns ana pelati
select a.contact_id, a.campaign_id
  from (
select contact_id, campaign_id
  from acts
  where year(act_date) <=2015
  group by contact_id, campaign_id ) as a
  inner
  join (
select contact_id, campaign_id
  from actions
  where year(action_date) <= 2015
  group by contact_id, campaign_id) as b
  on  a.contact_id = b.contact_id
  and a.campaign_id = b.campaign_id ) as c
   on c.contact_id = actions.contact_id
  and c.campaign_id = actions.campaign_id
  and year(actions.action_date) <= 2015
  group by actions.contact_id, actions.campaign_id ) as d
  group by contact_id
 -- order by 3 desc  
                     )  x
   on assignment2.contact_id = x.contact_id
;

############## Finished with solicitation measurements ##########################

drop table ass3_train_info_DO_2018;
# It actually has data from 2016
CREATE TABLE ass3_train_info_DO_2018 (
  `contact_id`           int(10) unsigned ,
  `DO_freq2018`          integer(5) DEFAULT NULL,
  `DO_min_amount_2018`   float DEFAULT NULL,
  `DO_max_amount_2018`   float DEFAULT NULL,
  `DO_total_amount_2018` float DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_info_DO_2018
select assignment2.contact_id   as contact_id,
       count(*)                 as DO_freq2018,
       ifnull(min(acts.amount), 0)  as DO_min_amount_2018, 
	   ifnull(max(acts.amount), 0)  as DO_max_amount_2018, 
       ifnull(sum(acts.amount),0)   as DO_total_amount_2018
  from ass3_contacts_train   assignment2
  left
  join acts  acts
    on acts.contact_id = assignment2.contact_id
   and acts.act_type_id = 'DO'
   and year(acts.act_date) = 2015
   group by assignment2.contact_id;
   
drop table ass3_train_info_PA_2018;
CREATE TABLE ass3_train_info_PA_2018 (
  `contact_id` int(10) unsigned ,
  `PA_freq2018` integer(5) DEFAULT NULL,
  `PA_min_amount_2018` float DEFAULT NULL,
  `PA_max_amount_2018` float DEFAULT NULL,
  `PA_total_amount_2018` float DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_info_PA_2018
select assignment2.contact_id,
       count(*) as PA_freq2018,     -- mou dinei 1 gia autous pou exoun pantou NULL -- provlima   , 1 exoun kai auti me mia mono sinallagi
       ifnull(min(acts.amount), 0)  as PA_min_amount_2018, 
	   ifnull(max(acts.amount), 0)  as PA_max_amount_2018, 
       ifnull(sum(acts.amount),0)   as PA_total_amount_2018
  from ass3_contacts_train   assignment2
  left
  join acts  acts
    on acts.contact_id = assignment2.contact_id
   and acts.act_type_id = 'PA'
   and year(acts.act_date) = 2015
   group by assignment2.contact_id;
   
 # select * from   ass3_train_info_PA_2018;

drop table ass3_train_DO_Average_amounts;
CREATE TABLE ass3_train_DO_Average_amounts (
  `contact_id` int(10) unsigned ,
  `avg_min_amount` float DEFAULT NULL,
  `avg_max_amount` float DEFAULT NULL,
  `avg_avg_amount` float DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_DO_Average_amounts
select assignment2.contact_id,
	   x.avg_min_amount,
       x.avg_max_amount,
       x.avg_avg_amount
  from ass3_contacts_train   assignment2
  left
  join (
select a.contact_id, 
       avg(min_amount)  as avg_min_amount, 
       avg(max_amount)  as avg_max_amount, 
       avg(avg_amount)  as avg_avg_amount
  from (
select contact_id, year(act_date), 
	   min(amount) as min_amount, 
       max(amount) as max_amount, 
       avg(amount) as avg_amount
  from acts
  where acts.act_type_id = 'DO' and year(act_date) <= 2015
  group by contact_id , year(act_date)
  order by 1, 2 ) as a
  group by a.contact_id ) as x
   on assignment2.contact_id = x.contact_id
  ;

drop table ass3_train_DO_Sum_amount_across_years;
 CREATE TABLE ass3_train_DO_Sum_amount_across_years (
  `contact_id` int(10) unsigned ,
  `min_sum_amount_across_years` float DEFAULT NULL,
  `max_sum_amount_across_years` float DEFAULT NULL,
  `avg_sum_amount_across_years` float DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;


insert into ass3_train_DO_Sum_amount_across_years
 select assignment2.contact_id , 
        x.min_sum_amount_across_years,
        x.max_sum_amount_across_years,
        x.avg_sum_amount_across_years
   from ass3_contacts_train   assignment2
   left 
   join (
  select a.contact_id, 
         min(sum_amount)  as min_sum_amount_across_years, 
         max(sum_amount)  as max_sum_amount_across_years, 
         avg(sum_amount)  as avg_sum_amount_across_years      -- , count(*) -- for check purpsoses
    from (
 select contact_id, year(act_date), sum(amount) as sum_amount
  from acts
   where acts.act_type_id = 'DO'  and year(act_date) <= 2015
  group by contact_id , year(act_date)
  order by 1, 2  
  ) as a
  group by a.contact_id ) as x
    on x.contact_id = assignment2.contact_id ;

drop table ass3_train_DO_Frequency_across_years;
   CREATE TABLE ass3_train_DO_Frequency_across_years (
  `contact_id` int(10) unsigned ,
  `min_freq_years_don` integer DEFAULT NULL,
  `max_freq_years_don` integer DEFAULT NULL,
  `avg_freq_years_don` integer DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_DO_Frequency_across_years
  select assignment2.contact_id,
         min_freq_years_don,
         max_freq_years_don,
		 avg_freq_years_don
    from ass3_contacts_train  assignment2
    left
    join (
  select a.contact_id, 
         min(acts_per_year)  as min_freq_years_don, 
         max(acts_per_year)  as max_freq_years_don, 
         avg(acts_per_year)  as avg_freq_years_don    -- , count(*)
    from (
 select contact_id, year(act_date), count(*) as acts_per_year
  from acts
   where acts.act_type_id = 'DO' and year(act_date) <= 2015
  group by contact_id , year(act_date)
  order by 1, 2  
  ) as a
  group by a.contact_id )    x
    on x.contact_id = assignment2.contact_id;


### I am here
drop table ass3_train_my_dataset_latest;
CREATE TABLE ass3_train_my_dataset_latest (
  `contact_id` int(10) unsigned ,
  `DO_recency` float,
  `DO_frequency` integer(5) DEFAULT NULL,
  `DO_avgamount` float DEFAULT NULL,
  `DO_firstdonation` float DEFAULT NULL,
  `DO_loyal` boolean,
  `PA_recency` float,
  `PA_frequency` integer(5) DEFAULT NULL,
  `PA_avgamount` float DEFAULT NULL,
  `PA_firstdonation` float DEFAULT NULL,
  `PA_loyal` boolean,
  `PA_active_flag` boolean,
  `Solicitation_Accuracy` float(7,4),
  `Sol_Accuracy1` float(7,4),
  `Sol_Accuracy2` float(7,4),
  `Averga_number_sol` integer(5) DEFAULT NULL,
   `DO_freq2018` integer(5) DEFAULT NULL,
  `DO_min_amount_2018` float DEFAULT NULL,
  `DO_max_amount_2018` float DEFAULT NULL,
  `DO_total_amount_2018` float DEFAULT NULL,
  `PA_freq2018` integer(5) DEFAULT NULL,
  `PA_min_amount_2018` float DEFAULT NULL,
  `PA_max_amount_2018` float DEFAULT NULL,
  `PA_total_amount_2018` float DEFAULT NULL,
   `avg_min_amount` float DEFAULT NULL,
  `avg_max_amount` float DEFAULT NULL,
  `avg_avg_amount` float DEFAULT NULL,
   `min_sum_amount_across_years` float DEFAULT NULL,
  `max_sum_amount_across_years` float DEFAULT NULL,
  `avg_sum_amount_across_years` float DEFAULT NULL,
  `min_freq_years_don` integer DEFAULT NULL,
  `max_freq_years_don` integer DEFAULT NULL,
  `avg_freq_years_don` integer DEFAULT NULL,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_my_dataset_latest
select my_dataset_PA_DO_Sol_label.contact_id                                   ,
      my_dataset_PA_DO_Sol_label.DO_recency                                   ,
      my_dataset_PA_DO_Sol_label.DO_frequency                                 ,
      my_dataset_PA_DO_Sol_label.DO_avgamount                                 ,
      my_dataset_PA_DO_Sol_label.DO_firstdonation                             ,
      my_dataset_PA_DO_Sol_label.DO_loyal                                     ,
      my_dataset_PA_DO_Sol_label.PA_recency                                   ,
      my_dataset_PA_DO_Sol_label.PA_frequency                                 ,
      my_dataset_PA_DO_Sol_label.PA_avgamount                                 ,
      my_dataset_PA_DO_Sol_label.PA_firstdonation                             ,
      my_dataset_PA_DO_Sol_label.PA_loyal                                     ,
      my_dataset_PA_DO_Sol_label.PA_active_flag                               ,
      my_dataset_PA_DO_Sol_label.Solicitation_Accuracy                        ,
       sol_accuracy_measures.Sol_Accuracy1                                   ,
       sol_accuracy_measures.Sol_Accuracy2       ,
       Solicitation_Efficiency.Averga_number_sol,        
       info_DO_2018.DO_freq2018         ,
       info_DO_2018.DO_min_amount_2018  ,
       info_DO_2018.DO_max_amount_2018  ,
       info_DO_2018.DO_total_amount_2018,
       info_PA_2018.PA_freq2018         ,
       info_PA_2018.PA_min_amount_2018  ,
       info_PA_2018.PA_max_amount_2018  ,
       info_PA_2018.PA_total_amount_2018,
      DO_Average_amounts.avg_min_amount ,
       DO_Average_amounts.avg_max_amount,
       DO_Average_amounts.avg_avg_amount,
       DO_Sum_amount_across_years.min_sum_amount_across_years,
       DO_Sum_amount_across_years.max_sum_amount_across_years,
       DO_Sum_amount_across_years.avg_sum_amount_across_years,
       DO_Frequency_across_years.min_freq_years_don ,
       DO_Frequency_across_years.max_freq_years_don  ,
       DO_Frequency_across_years.avg_freq_years_don                                  
  from ass3_train_my_dataset_pa_do_sol_label  my_dataset_pa_do_sol_label
  left
  join ass3_train_sol_accuracy_measures  sol_accuracy_measures
    on sol_accuracy_measures.contact_id = my_dataset_pa_do_sol_label.contact_id
  left
  join ass3_train_Solicitation_Efficiency  Solicitation_Efficiency
    on Solicitation_Efficiency.contact_id = my_dataset_pa_do_sol_label.contact_id
  left
  join ass3_train_info_DO_2018   info_DO_2018
    on info_DO_2018.contact_id = my_dataset_pa_do_sol_label.contact_id
  left
  join ass3_train_info_PA_2018   info_PA_2018
    on info_PA_2018.contact_id = my_dataset_pa_do_sol_label.contact_id
 left
 join ass3_train_DO_Average_amounts   DO_Average_amounts
   on DO_Average_amounts.contact_id = my_dataset_pa_do_sol_label.contact_id
 left
 join ass3_train_DO_Sum_amount_across_years  DO_Sum_amount_across_years
   on DO_Sum_amount_across_years.contact_id = my_dataset_pa_do_sol_label.contact_id
 left
 join ass3_train_DO_Frequency_across_years  DO_Frequency_across_years
   on DO_Frequency_across_years.contact_id = my_dataset_pa_do_sol_label.contact_id;
   
   
 --------------- Round 3 ----------------- 
 drop table ass3_train_Customer_Active;
 CREATE TABLE ass3_train_Customer_Active (
  `contact_id` int(10) unsigned ,
  `Customer_active` boolean,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

-- bring active
insert into ass3_train_Customer_Active
select assignment2.contact_id , contacts.active
  from ass3_contacts_train  assignment2
  left
  join contacts  contacts
    on contacts.id = assignment2.contact_id
  ;
  

-- prefixes
  drop table ass3_train_Prefix_table;
	CREATE TABLE ass3_train_Prefix_table (
  `contact_id` int(10) unsigned ,
  `Autre` boolean,
  `Docteur` boolean,
  `Maitre` boolean,
  `Mademoiselle` boolean,
  `Madame` boolean,
  `Monsieur_et_Madame` boolean,
  `Monsieur` boolean,
  `NA` boolean,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM; 

insert into ass3_train_Prefix_table
select assignment2.contact_id as contact_id,
       (case when(contacts.prefix_id = "AU") then 1
	   else 0 end)   as Autre,
	   (case when(contacts.prefix_id = "DR") then 1
       else 0 end) as Docteur,
       (case when(contacts.prefix_id = "ME") then 1
       else 0 end) as Maitre,
	   (case when(contacts.prefix_id = "MLLE") then 1
       else 0  end) as Mademoiselle,
	   (case when(contacts.prefix_id = "MME") then 1
       else 0  end) as Madame,
	   (case when(contacts.prefix_id = "MMME") then 1
       else 0 end)  as Monsieur_et_Madame,
	   (case when(contacts.prefix_id = "MR") then 1
       else 0 end)  as Monsieur,
	   (case when(contacts.prefix_id = "NA") then 1
       else 0  end) as NA
-- prefixes.id, prefixes.label,  count(distinct assignment2.contact_id)
  from ass3_contacts_train  assignment2
  left
  join contacts  contacts
    on contacts.id = assignment2.contact_id
;


select sum(Autre), sum(Docteur), sum(Maitre), sum(Mademoiselle), sum(Madame), sum(Monsieur_et_Madame), sum(Monsieur), sum(NA)
  from ass3_contacts_train   assignment2 
  left
  join ass3_train_Prefix_table  prefix_table
    on assignment2.contact_id = prefix_table.contact_id; 
  

-- Customers channel usage

drop table ass3_train_Customer_channel_cat;
CREATE TABLE ass3_train_Customer_channel_cat (
  `contact_id`        int(10) unsigned ,
  `Mailing`           integer,
  `Internet`          integer,
  `Street`            integer,
  `Telephone`         integer,
  `Evenement`         integer,
  `Quete`             integer,
  `Cheque`            integer,
  `Prelevement`       integer,
  `Cart_Bancaire`     integer,
  `Virement_Bancaire` integer,
  `Especes`           integer,
  `Autre`             integer,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM; 



insert into ass3_train_Customer_channel_cat
select a.contact_id,
       sum(a.Mailing)  as Mailing,
       sum(a.Internet)  as Internet,
       sum(a.Street)  as Street,
       sum(a.Telephone)  as Telephone,
       sum(a.Evenement)  as Evenement,
       sum(a.Quete)  as Quete,
	   sum(a.Cheque)  as Cheque,
       sum(a.Prelevement)  as Prelevement,
       sum(a.Cart_Bancaire)  as Cart_Bancaire,
       sum(a.Virement_Bancaire)  as Virement_Bancaire,
       sum(a.Especes)  as Especes,
       sum(a.Autre)  as Autre
  from (
select assignment2.contact_id,
       (case when(acts.channel_id = "MA") then 1
	   else 0 end)   as Mailing,
       (case when(acts.channel_id = "WW") then 1
	   else 0 end)   as Internet,
       (case when(acts.channel_id = "ST") then 1
	   else 0 end)   as Street,
       (case when(acts.channel_id = "TE") then 1
	   else 0 end)   as Telephone,
       (case when(acts.channel_id = "EV") then 1
	   else 0 end)   as Evenement,
       (case when(acts.channel_id = "QU") then 1
	   else 0 end)   as Quete,
	   (case when(acts.payment_method_id = "CH") then 1
	   else 0 end)   as Cheque,
	   (case when(acts.payment_method_id = "PR") then 1
	   else 0 end)   as Prelevement,
	   (case when(acts.payment_method_id = "CB") then 1
	   else 0 end)   as Cart_Bancaire,
	   (case when(acts.payment_method_id = "VI") then 1
	   else 0 end)   as Virement_Bancaire,
       (case when(acts.payment_method_id = "ES") then 1
	   else 0 end)   as Especes,
	   (case when(acts.payment_method_id = "AU") then 1
	   else 0 end)   as Autre
  from ass3_contacts_train   assignment2 
  left
  join acts acts
    on acts.contact_id = assignment2.contact_id
    where year(acts.act_date) <= 2015 ) as a
  group by contact_id;

drop table ass3_train_my_dataset_latest2;
  
CREATE TABLE ass3_train_my_dataset_latest2 (
  `contact_id` int(10) unsigned ,
  `DO_recency` float,
  `DO_frequency` integer(5) DEFAULT NULL,
  `DO_avgamount` float DEFAULT NULL,
  `DO_firstdonation` float DEFAULT NULL,
  `DO_loyal` boolean,
  `PA_recency` float,
  `PA_frequency` integer(5) DEFAULT NULL,
  `PA_avgamount` float DEFAULT NULL,
  `PA_firstdonation` float DEFAULT NULL,
  `PA_loyal` boolean,
  `PA_active_flag` boolean,
  `Sol_Accuracy1` float(7,4),
  `Sol_Accuracy2` float(7,4),
  `Averga_number_sol` integer(5) DEFAULT NULL,
   `DO_freq2018` integer(5) DEFAULT NULL,
  `DO_min_amount_2018` float DEFAULT NULL,
  `DO_max_amount_2018` float DEFAULT NULL,
  `DO_total_amount_2018` float DEFAULT NULL,
  `PA_freq2018` integer(5) DEFAULT NULL,
  `PA_min_amount_2018` float DEFAULT NULL,
  `PA_max_amount_2018` float DEFAULT NULL,
  `PA_total_amount_2018` float DEFAULT NULL,
   `avg_min_amount` float DEFAULT NULL,
  `avg_max_amount` float DEFAULT NULL,
  `avg_avg_amount` float DEFAULT NULL,
   `min_sum_amount_across_years` float DEFAULT NULL,
  `max_sum_amount_across_years` float DEFAULT NULL,
  `avg_sum_amount_across_years` float DEFAULT NULL,
  `min_freq_years_don` integer DEFAULT NULL,
  `max_freq_years_don` integer DEFAULT NULL,
  `avg_freq_years_don` integer DEFAULT NULL,
  `Contact_Active` boolean,
  `Channel_Autre` boolean,
  `Docteur` boolean,
  `Maitre` boolean,
  `Mademoiselle` boolean,
  `Madame` boolean,
  `Monsieur_et_Madame` boolean,
  `Monsieur` boolean,
   `Mailing` integer,
  `Internet` integer,
  `Street` integer,
  `Telephone` integer,
  `Evenement` integer,
  `Quete` integer,
  `Cheque` integer,
  `Prelevement` integer,
  `Cart_Bancaire` integer,
  `Virement_Bancaire` integer,
  `Especes` integer,
  `Autre` integer,
  PRIMARY KEY (`contact_id`),
  INDEX idx_contact_id(contact_id)
) ENGINE=MyISAM;

insert into ass3_train_my_dataset_latest2
select 
       my_dataset_latest.contact_id                                   ,
       my_dataset_latest.DO_recency                                   ,
       my_dataset_latest.DO_frequency                                 ,
       my_dataset_latest.DO_avgamount                                 ,
       my_dataset_latest.DO_firstdonation                             ,
       my_dataset_latest.DO_loyal                                     ,
       my_dataset_latest.PA_recency                                   ,
       my_dataset_latest.PA_frequency                                 ,
       my_dataset_latest.PA_avgamount                                 ,
       my_dataset_latest.PA_firstdonation                             ,
       my_dataset_latest.PA_loyal                                     ,
       my_dataset_latest.PA_active_flag                               ,
       my_dataset_latest.Sol_Accuracy1                                   ,
       my_dataset_latest.Sol_Accuracy2       ,
       my_dataset_latest.Averga_number_sol,        
       my_dataset_latest.DO_freq2018         ,
       my_dataset_latest.DO_min_amount_2018  ,
       my_dataset_latest.DO_max_amount_2018  ,
       my_dataset_latest.DO_total_amount_2018,
       my_dataset_latest.PA_freq2018         ,
       my_dataset_latest.PA_min_amount_2018  ,
       my_dataset_latest.PA_max_amount_2018  ,
       my_dataset_latest.PA_total_amount_2018,
       my_dataset_latest.avg_min_amount ,
       my_dataset_latest.avg_max_amount,
       my_dataset_latest.avg_avg_amount,
       my_dataset_latest.min_sum_amount_across_years,
       my_dataset_latest.max_sum_amount_across_years,
       my_dataset_latest.avg_sum_amount_across_years,
       my_dataset_latest.min_freq_years_don ,
       my_dataset_latest.max_freq_years_don  ,
       my_dataset_latest.avg_freq_years_don  ,
      Customer_Active.Customer_Active  ,
      Prefix_table.Autre,              
      Prefix_table.Docteur,   
      Prefix_table.Maitre , 
      Prefix_table.Mademoiselle,       
      Prefix_table.Madame,             
      Prefix_table.Monsieur_et_Madame,
      Prefix_table.Monsieur,       
Customer_channel_cat.Mailing            ,
Customer_channel_cat.Internet           ,
Customer_channel_cat.Street             ,
Customer_channel_cat.Telephone          ,
Customer_channel_cat.Evenement          ,
Customer_channel_cat.Quete              ,
Customer_channel_cat.Cheque             ,
Customer_channel_cat.Prelevement        ,
Customer_channel_cat.Cart_Bancaire      ,
Customer_channel_cat.Virement_Bancaire  ,
Customer_channel_cat.Especes            ,
Customer_channel_cat.Autre       
  from ass3_train_my_dataset_latest  my_dataset_latest
  left
  join ass3_train_Customer_Active     Customer_Active
    on Customer_Active.contact_id = my_dataset_latest.contact_id
  left
  join ass3_train_Prefix_table       Prefix_table
    on Prefix_table.contact_id = my_dataset_latest.contact_id
  left
  join ass3_train_Customer_channel_cat   Customer_channel_cat
    on Customer_channel_cat.contact_id = my_dataset_latest.contact_id
    order by my_dataset_latest.contact_id  asc;
     
select count(*) from ass3_train_my_dataset_latest2;  # 91315


select ass3_train_my_dataset_latest2.contact_id, avg(acts.amount) as avg_amount
  from ass3_train_my_dataset_latest2  ass3_train_my_dataset_latest2
  left 
  join acts   acts
    on ass3_train_my_dataset_latest2.contact_id = acts.contact_id
  and year(acts.act_date) = 2016
  group by ass3_train_my_dataset_latest2.contact_id
  order by avg(acts.amount)  ;   # 91315
  
 

