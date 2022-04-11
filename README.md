# hypothesis_testing_jeremy_fabian
original app:
https://psu-eberly.shinyapps.io/Hypothesis_Testing/

original code:
https://github.com/EducationShinyAppTeam/Hypothesis_Testing

The overall goal of this project was to create a scaffold for the BOAST hypothesis testing app. We believe the core of BOAST's app 
is a great way of explaining hypothesis testing, but the app itself is overloaded. So we added more steps and simplified the app. We assume that the user is an undergraduate/ graduate student with a basic understanding of statistics. The user should have heard about distribution, mean and standard deviation. 

The overview page states the learning objectives and sets up the scenario: comparing two basketball players based on their free throws. 
The graph indicates that Player B is the better player (it's true proportion of hits value is 0.4. Player A has a true value of 0.6).
At this point the learner does not know this. The learner leaves the first page with a question in mind: Who is the better player?

The "sampling explained" page is the scaffold, which will lead up to the original BOAST app. We hope the learner gets more out of the original boast hypothesis testing app after completing the “sampling explained” page. 

The first simulation illustrates that a Free Throw sample consists of hits and misses. Those shape a binominal distribution. We do not highlight it here yet, but the attentive learner might already notice that the standard deviation shrinks when the number of shots increases. 

The second simulation is the heart of the comparison. The intention of this simulation is to show, that low sample sizes, numbers of free throws, do not allow for a comparison. With only 10 throws Player A wins in some cases and in other cases Player B does. It is not clear who is better. This situation changes by increasing the number of throws. It becomes obvious that Player A is better. Furthermore, this simulation highlights the importance of the standard deviation. The whiskers grow and shrink according to the number of throws. 

The third page, hypothesis testing, is a simpler version of the BOAST app. The idea is, that the learner estimates the true value of the players (Player A = .6, Player B = .4). A hypothesis test will tell the learner if the estimate is likely to be correct. The learner has to interpret the p-value. 
