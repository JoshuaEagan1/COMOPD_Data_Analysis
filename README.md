# Columbia (Missouri) Police Department Data Analysis

These projects will shed light on different aspects of traffic policing in Columbia, Missouri. The goal of this project is to make data and insights from the traffic stops data released by the Columbia Police Department (COMOPD) easily accessible. My hope is that this will provide a common understanding between citizens and law enforcement to serve as a starting point for a conversation about what criminal justice enforcement should look like in Columbia, Missouri.

## Projects

There are a few main products I will make available to COMOPD, Columbia's Civilian Police Review Board, and the public. 

### COMOPD Data Dashboard
I am designing a data dashboard that will empower people who want to know more about what traffic stops look like to explore the data in user friendly, customizable environment.

### Veil of Darkness Analysis
I will be releasing a report that analyses Columbia's traffic stops data for racial discrimination against black drivers. This test leverages as-good-as-random variation in visibility between daylight and darkness to test for discrimination against black motorists. Controlling for where traffic stops take place and when they take place, officers should be less able to observe the race of motorists just after sunset relative to just before sunset. Therefore, if the likelihood that a stopped driver is black decreases significantly after sunset, this is evidence of racial discrimination against black motorists.

One way this approach could be furthered (in a way that has not been tried by other researchers) is to test for vehicle discrimination. If vehicle type is correlated with race and officers are able to predict race based on observing the exterior of a vehicle, then Veil of Darkness analysis would be biased toward finding null results. If I am able to obtain information about the type of vehicle stopped (make, model, etc.) I could check for vehicle discrimination and control for it in the Veil of Darkness model.

### Saturation Patrols
One of the steps that Columbia Police Department has taken to try to reduce the racial disparity in traffic stops has been to ban the practice of saturation patrols. These patrols consist of concentrating officers in a small, high crime region of a city as a crime deterrent. Columbia Police Department revised policy 500.4 to reflect this change, adding the following language "Blanket saturations other than DWI saturations are not permitted. Any traffic stop conducted by a member of the Columbia Police Department, including those made during a targeted enforcement activity, will be based on articulable reasonable suspicion that a crime or traffic offense infraction has been or is being committed." Anecdotally, these patrols have been concentrated in police beat 20, the beat with the highest percentage of the population that is black by a large margin (over 30%). In February of 2019, this patrolling tactic was effectively eliminated on principle.

I hope to uncover whether this had any effect on search hit rates, the percentage of searches that lead to finding illegal contraband, in the areas of Columbia where these patrols occurred. In order to do this, I will be trying to assess exactly when the policy change occurred (using the traffic stops data to validate this threshold.)

