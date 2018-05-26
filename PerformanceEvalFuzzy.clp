;Employee Performance Evaluation System
;Fuzzy Variables
;(load-package nrc.fuzzy.jess.FuzzyFunctions)

(defglobal ?*company-years* = 
    (new nrc.fuzzy.FuzzyVariable "company-years" 0.0 102.0 "years"))
(defglobal ?*industry-experience* =
    (new nrc.fuzzy.FuzzyVariable "industry-experience" 0.0 50.0 "years"))
(defglobal ?*currentteam-exp* =
    (new nrc.fuzzy.FuzzyVariable "currentteam-exp" 0.0 50.0 "years"))
(defglobal ?*rating* =
    (new nrc.fuzzy.FuzzyVariable "rating" 0.0 100.0 "score"))


(defrule initial-terms
    (declare (salience 100))
=>
(import nrc.fuzzy.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)

;Primary Terms

(?*company-years* addTerm "low" (new ZFuzzySet 50.00 80.00))
(?*company-years* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 34.00 44.00 55.00))
(?*company-years* addTerm "high" (new SFuzzySet 16.00 35.00))

(?*industry-experience* addTerm "low" (new ZFuzzySet 1.00 6.00))
(?*industry-experience* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 4.00 8.00 12.00))
(?*industry-experience* addTerm "high" (new SFuzzySet 10.00 40.00))

(?*currentteam-exp* addTerm "low" (new ZFuzzySet 1.00 6.00))
(?*currentteam-exp* addTerm "medium" (new nrc.fuzzy.TriangleFuzzySet 4.00 8.00 12.00))
(?*currentteam-exp* addTerm "high" (new SFuzzySet 10.00 40.00))

    
(?*rating* addTerm "low" (new ZFuzzySet 3.0 5.0))
(?*rating* addTerm "medium" (new PIFuzzySet 6.0 4.0))
(?*rating* addTerm "high" (new SFuzzySet 7.0 10.0))
)


;Startup module

(defrule welcome-user
(declare(salience 50))
=>

(printout t "Welcome to the Performance Evaluation System!" crlf)
(printout t "Type employee's name press Enter> "crlf)
(bind ?name (read))
(printout t "Performance evaluation will now begin for " ?name "." crlf)
(printout t "Please answer the below questions and" crlf)
(printout t "the system will provide the performance rating and suggestions." crlf))


;Initialization

;The answer must be between the values mentioned in the question,as the values have to be within the universe of discourse.
(defrule assert-answers "initialization"
=>
    (printout t "Number of years the employee has been in the company? "crlf"The answer must be above 2 and below 100"crlf)
    (bind ?c-value (float (readline t)))
	(printout t "How many years of experience does the employee have in this domain? "crlf "The answer must be above 0.5 and below 50"crlf)
    (bind ?i-value (float (readline t)))
    (printout t "How many years of experience does the employee have in the current team"crlf"and the current project?" crlf "The answer must be above 0.5 and below 50"crlf)   
    (bind ?cu-value (float (readline t)))
    (printout t "What is the rating of the employee in terms of their knowledge and skillset in the domain?"crlf"The answer must be in the range(1-10)"crlf)   
    (bind ?skillset (float (readline t)))
    (printout t "What is the rating of the employee in terms of accuracy and thoroughness in their work?"crlf"The answer must be in the range(1-10)"crlf)   
    (bind ?accuracy-rating (float (readline t)))
    (printout t "What is the rating of the employee in terms of their analytical ability?"crlf"The answer must be in the range(1-10)"crlf)
    (bind ?ability-rating (float (readline t)))
    (printout t "What is the rating of the employee given by the team members for team work?"crlf"The answer must be in the range(1-10)"crlf)
    (bind ?team-rating (float (readline t)))
    (printout t "What is the rating of the employee,given by the employee's manager?"crlf"The answer must be in the range(1-10)"crlf)
    (bind ?manager-rating (float (readline t)))


    (assert(companyYears
        (new nrc.fuzzy.FuzzyValue ?*company-years*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?c-value 2.0)
        ?c-value
        (+ ?c-value 2.0)))))
    (assert(industryExperience
        (new nrc.fuzzy.FuzzyValue ?*industry-experience*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?i-value 0.5)
        ?i-value
        (+ ?i-value 0.5)))))
    (assert(currentTeamExp
        (new nrc.fuzzy.FuzzyValue ?*currentteam-exp*
        (new nrc.fuzzy.TriangleFuzzySet
        (- ?cu-value 0.5)
        ?cu-value
        (+ ?cu-value 0.5)))))
    
    
    (assert(skillsetRating ?skillset))
    (assert(accuracyRating ?accuracy-rating))
    (assert(abilityRating ?ability-rating))
    (assert(teamRating  ?team-rating))
    (assert(managerRating ?manager-rating)))


;Fuzzy Rules for calculation


(defrule rating-set1 ;"low years in company & low industry experience & low experience in project=> rating very low or low"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "very low or low"))))

(defrule rating-set2 ;"low years in company & high industry experience & low experience in project => rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule rating-set3 ;"low years in company & medium industry experience & low experience in project => rating low or medium"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium"))))

(defrule rating-set4 ;"high years in company & high industry experience & low experience in project=> rating high or medium or low"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule rating-set5 ;"high years in company & low industry experience & low experience in project => rating high or medium or low"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule rating-set6 ;"high years in company & medium industry experience & low experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule rating-set7 ;"medium years in company & high industry experience & low experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule rating-set8 ;"medium years in company & medium industry experience & low experience in project=> rating low or medium"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* " low or medium "))))

(defrule rating-set9 ;"medium years in company & low industry experience & low experience in project=> rating medium or low"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "medium or low"))))

(defrule rating-set10 ;"low years in company & low industry experience & medium experience in project=> rating very low or low or medium"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "very low or low or medium"))))

(defrule rating-set11 ;"low years in company & high industry experience  & medium experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule rating-set12 ;"low years in company & medium industry experience  & medium experience in project=> rating low or medium"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium"))))

(defrule rating-set13 ;"high years in company & high industry experience  & medium experience in project=> rating high or medium"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium"))))

(defrule rating-set14 ;"high years in company & low industry experience  & medium experience in project=> rating high or medium or low"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule rating-set15 ;"high years in company & medium industry experience  & medium experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "low"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule rating-set16 ;"medium years in company & high industry experience  & medium experience in project=> rating medium or high"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* " medium or high"))))

(defrule rating-set17 ;"medium years in company & medium industry experience  & medium experience in project=> rating medium"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* " medium "))))

(defrule rating-set18 ;"medium years in company & low industry experience  & medium experience in project=> rating medium or low"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "medium"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "medium or low"))))

(defrule rating-set19 ;"low years in company & low industry experience  & high experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* " low or medium or high"))))

(defrule rating-set20 ;"low years in company & high industry experience & high experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule rating-set21 ;"low years in company & medium industry experience & high experience in project=> rating low or medium or high"
    (companyYears ?a &: (fuzzy-match ?a "low"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "low or medium or high"))))

(defrule rating-set22 ;"high years in company & high industry experience & high experience in project=> rating very high or high"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or very high"))))

(defrule rating-set23 ;"high years in company & low industry experience & high experience in project=> rating high or medium or low"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium or low"))))

(defrule rating-set24 ;"high years in company & medium industry experience & high experience in project=> rating medium or high"
    (companyYears ?a &: (fuzzy-match ?a "high"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "high or medium"))))

(defrule rating-set25 ;"medium years in company & high industry experience & high experience in project=> rating medium or high"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "high"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "medium or high"))))

(defrule rating-set26 ;"medium years in company & medium industry experience & high experience in project=> rating medium or high"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "medium"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* " medium or high "))))

(defrule rating-set27 ;"medium years in company & low industry experience & high experience in project=> rating medium or low or high"
    (companyYears ?a &: (fuzzy-match ?a "medium"))
    (industryExperience ?e &: (fuzzy-match ?e "low"))
    (currentTeamExp ?r &: (fuzzy-match ?r "high"))
=>
    (assert(Rating (new nrc.fuzzy.FuzzyValue ?*rating* "medium or low or high"))))

;Defuzzification


(defrule defuzzification-and-display
    (declare (salience -1))
    ?f <- (Rating ?z)
    (skillsetRating ?s)
    (accuracyRating ?u)
    (abilityRating ?b)
   	(teamRating ?t)
    (managerRating ?m)
=>
    (str-cat "Rating: " (?z momentDefuzzify))
	(bind ?calculated-rating (integer (+ (* 0.20 ?s) (* 0.15 ?u) (* 0.15 ?b)  (* .15 ?t) (* .15 ?m) (* .20 (?z momentDefuzzify)))))
    (if(> ?calculated-rating 10) then
    (printout t "Rating : 10" crlf)
    else 
    (printout t "Rating": ?calculated-rating crlf))
    (if (eq ?calculated-rating 5) then
    (printout t "Comments:"crlf"The holistic rating of employee is below average."crlf"Suggestion:"crlf"The employee can improve their rating by" crlf "gaining a deeper insight of the domain in which they are working."crlf)
    elif(eq ?calculated-rating 6) then
    (printout t "Comments:"crlf"The holistic rating of the employee is average."crlf"Suggestion:"crlf"The employee can improve their rating" crlf"by enhancing their skills and by improving their working style,in the team."crlf)    
    elif(eq ?calculated-rating 7) then
    (printout t "Comments:"crlf"The holistic rating of the employee is above average."crlf"Suggestion"crlf"The employee can improve their rating" crlf"by paying closer attention to details."crlf)
    elif(eq ?calculated-rating 8) then
    (printout t "Comments:"crlf"The employee has a good holistic rating." crlf"Suggestion:"crlf"The employee can set up a meeting with their manager"crlf"to determine the minor intricacies they"crlf"need to focus on to improve their rating!"crlf)
    elif(eq ?calculated-rating 9) then
    (printout t "Comments:"crlf"The employee has a great holistic rating!" crlf"The employee is very productive in the team."crlf"The employee is also subjected to a raise in the annual salary!"crlf)
    elif(eq ?calculated-rating 10) then
    (printout t "Comments:"crlf"The employee has a great holistic rating!"crlf"The employee is a star in the team and is " crlf"subjected to an annual raise in the salary!"crlf)
    elif(> ?calculated-rating 10) then
    (printout t "Comments:"crlf"The employee has a great holistic rating!"crlf"The employee is very resourceful in the team and is" crlf"subjected to a raise in the salary annually"crlf"Congratulations!"crlf)
    else
    (printout t "Comments:"crlf"The employee has a low holistic rating."crlf"Suggestion:"crlf"The employee must work hard to improve their rating." crlf"They can get help from the team to enhance their skillset."crlf"They can set up meetings with their manager frequently."crlf))  
    
    (halt))

;function to run the application

(deffunction run-application ()
(reset)
(run))

;Runs the above function in a loop to get back the prompt every time we have to enter the values for another employee or re-run the program

(while TRUE
(run-application))


