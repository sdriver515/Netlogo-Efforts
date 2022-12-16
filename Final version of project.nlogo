;This code is mostly a refactorization of the model
; https://www.openabm.org/model/3051/version/1/view
; by Jula Schindler, who published a supporting paper
; at http://jasss.soc.surrey.ac.uk/15/1/4.html
;Schindler's model adds socio-psychological considerations
; to a model of the Tragedy of the Commons.
; This refactorization is by Alan G. Isaac.

;Notes for TC01:
; You need to document the meaning of every variable.

;This is Sarah making a comment. My initials for comment explanations are S.A.D.
;This is Nouf, My initial for the comments is NB

;
; see Schindler paper to document type and meaning of globals

;Comments about code additions:
;Sarah made two procedures, seefriendshipsandMove and sharesteakwithFriends.
;She also added the global pLink variable and undirected links for friendships, then set herder friends, colored then a different shade of gray, and hid the links.
;Her procedures can be used in BehaviorSpace, and they are tied to buttons on the Interface.
;The seefriendshipsandMove procedure looks for linked friendships by color and then randomly sometimes moves friends that are too close.
;The sharesteakwithFriends procedure looks for friends that own a lot of cows. They decide to have a feast (eat the cows) because friends like sharing. Some of the cows randomly die.
;She also made a test procedure for sharessteakswithFriends, called test-sharesteakwithFriends.
;Michael made the test for the seefriendshipsandMove procedure, called test-seeFriends.
;Michael creaated a drought procedure too.
;Nouf created a different type of moving-if-one-has-friends procedure. She used purely links instead of colors.
;Nouf also created a procedure for recalulcating grows per the weather, and a test for it. There is a Chooser for it on the interface.
;Madison created a procedure that visually reflects reward amounts onto the herders. She also created a test for cost.

globals [
  pLink ;S.A.D. this is for friendship procedures and it is set to 0.10; integer
]
;
; See Schindler paper to document interface globals and their meaning (see Table 1 of paper).
; A few variables have been renamed as follows:
; cow-forage-requirement -> cowForageRequirement
; total-cows -> nCowsInitial
; initial-grassland -> pctGrassInitial
; Cow-price -> cowPrice
; Also, the number of herders is now nHerdersInitial (no longer hardcoded at 6)
patches-own [
  grass        ;Integer (0 | 1)
]
; see Schindler paper to document the type and meaning of attributes
breed [herders herder]
herders-own [
  dHerd ;Integar (-1 | 1)
  pAddCow
]
breed [cows cow]
cows-own [
  forage
]
directed-link-breed [owners owner]
undirected-link-breed [friendships friends] ;S.A.D. this is for friendship procedures

to go
  if ticks = 300 [stop] ;MB - sets an automatic maximum run time in the simulation.
  step
  updateGUI
  ;MB - to go minimizes clunkiness of a simulation & no repeating of code
end

to randomize
  random-seed 314 ;S.A.D. used this for one of Sarah's experiments
end

;ai: try to document the baseline by reference to Schindler's paper
to startup
  set-default-shape cows "cow"
  set-default-shape herders "person"
  ;Set parameters at baseline
  ;Baseline is the SCEN-P-0 from the Schindler Model - MB
  set nHerdersInitial 4
  set nCowsInitial 120
  set pctGrassInitial 100
  set maxGrowthrateGrass 0.0010
  set cowForageRequirement 1


  ;Set dispositions at baseline
  set selfishness 1   ;MB - Schindler model sets selfishness to 1
  set cooperativeness 0
  set fairness-to-myself 0
  set fairness-to-others 0
  set positive-reciprocity 0
  set negative-reciprocity 0
  set conformity 0
  set risk-aversion 0

  ;MCH drought function globals
  set probDrought 0.05             ;MCH sets value of global variable probDrought and sets default value to 0.05
                                   ;represents probability of drought
                                   ;float [0,1]
  set sevDrought 0.005             ;MCH sets value of global variable sevDrough and sets default value to 0.005
                                   ;represents severity of drought
                                   ;float [0,0.10]


end

to setup
  ca
  ; set up initial grass cover
  let _nPatches round (pctGrassInitial / 100 * count patches) ;S.A.D. This means that a local variable was created called _nPatches. It is the rounded % of initial grass divided by 100 multiplied by the patches count.
  ask (n-of _nPatches patches) [set grass 1] ;S.A.D. Ask (however many are above) to be randomly chosen and set to grass 1.
  ; set up herders
  create-herders nHerdersInitial [
    set dHerd one-of [-1 1]
    set pAddCow 0.5
  ]
  ; set up cows
  create-cows nCowsInitial [
    create-owner-from one-of herders [hide-link] ;MCH select random
  ]
  set pLink 0.1 ;S.A.D: set global variable's probability of linking friendship (10%)
  reset-ticks
  random-seed behaviorspace-run-number ;turn on when you want to everything identical
  setupGUI
  ask herders [create-friendships-with other herders with [self < myself and random-float 1 < pLink] [ask both-ends [set color gray - 1]]] ;S.A.D.: create friendships with probability less than pLink % and make them a darker shade of gray
  ask herders [ask my-friendships [hide-link]] ;S.A.D. this hides the linked friendship lines between herders
end

to setupGUI
  ask herders [move-to one-of patches] ;S.A.D. Ask herders to move to one of the patches
  ask herders [set color gray set size 2] ;S.A.D. Ask herders to be set to size 2 and be gray in color
  ask cows [move-to one-of patches] ;MB - asks cows to move to a random (one-of) patch
  updateGUI
end

to seefriendshipsandMove ;S.A.D. herders-with-friends know which herders have friends. They value people with friends, even if not their own friends. They attempt to give them space (though this doesn't always succeed because there is an element of randomness to this).
  ask patches [if any? herders in-radius 5 with [color = gray - 1] [set pcolor green - 1]] ;if any herder with a friendship is in a radius of 5, change patch colors to dark green
  ask herders with [color = gray - 1] [ ;note: this shade of gray signifies that they have friends
    if count herders with [color = gray - 1] in-radius 10 > 1 [move-to one-of patches with [pcolor = green]]] ;ask herders-with-friendships that are too close to other herders-with-friendships to try to move away from them
  ask patches [set pcolor ifelse-value (grass > 0) [green] [brown]] ;this reverts cows to correct colors
  ask herders [set color gray] ;this reverts herders to correct colors
end

to sharesteakwithFriends ;S.A.D. herders with friendships and large herds eat random cows during a feast
  if (any? owner-neighbors and any? herders with [count my-friendships > 0]) [ ;If there are owned cows and herders with friendships, then...
    ask herders with [count my-friendships > 0 and count owner-neighbors > 25] [ ;ask the herders with friendships and lots of cows to:
      ask (one-of owner-neighbors) [die]]] ;have a random cow/s from their herd be eaten (aka die)
end

;Nouf also made a slightly different version of a see-friendships-and-move procedure
to seefriendshipsandMove02 ;herders-with-friends know which herders have friends. They value people with friends, even if not their own friends.
  ask herders [
  let my-friends out-friends-neighbors
  let my-available-patches patches in-radius 10 with [pcolor = green]
  if count my-available-patches > 0 [
  ask my-friends  [
   move-to one-of my-available-patches]] ;
  ]
  ask patches [set pcolor ifelse-value (grass > 0) [green] [brown]] ;this reverts cows to correct colors
  ask herders [set color gray] ;this reverts herders to correct colors
end

to rewardVisual ;MB
  ;Visually shows the reward effect on herders
  ask herders with [reward > 1.5] [set color red] ;when reward is more than 1.5 herders are more likely to take advatange of the commons
  ask herders with [reward < 1.5] [set color blue]
end

to test-rewardVisual
end

to-report recalculateGrows [nGrow] ; this procedure to recalculate grows depending on weather condition NB

  if weather = "raining" [ ; if weather is rainy we increase number of grows patches
    set nGrow nGrow * 2
  ]

  if weather = "dry" [ ; if weather is dry devide by 2
    set nGrow nGrow / 2
  ]

  ;no change if weather is normal
  report nGrow
end

to step                       ;MCH function pops to next pointer t + 1
;  drought                    ;MCH not part of the original model, but this is how I would implement to add it into the model. Commented out to not interfere with the current model.
  regrowGrass                 ;MCH call the function regrowGrass to set up grass patches between each step based on a formula using local variables
                              ;MCH related to max growth rate,
                              ;MCH where grass is an attribute of patches, where {grass present, grass absent} = {1,0}

  ask cows [graze]            ;MCH ask turtles named cows to execute function graze which makes cows move to nearest patch of grass
                              ;MCH and when they reach it set the grass attribute of the patch to 0.

  ask herders [learn]         ;MCH ask turtles named herders to execute function learn, which depends on function reward
                              ;MCH that inputs socio-psychological dispositions into the function learn
                              ;MCH this determines whether the herders gain cows from their learning or lose cows from their lack of learning

  ask herders [update-stock]  ;MCH ask turtles named herders to execute function update-stock which changes herd size and owner of herd.
  tick                        ;MCH adds 1 to the running count.
end

to drought                                                           ;MCH creates drought at some probability which reduces maxGrowthrateGrass for one tick
                                                                     ;implementation into model requires placing this function in the step function before regrowGrass
                                                                     ;if it is not in the step function, it will not work as intended
                                                                     ;currently, I commented it out in the step function so that it does not affect the original model
                                                                     ;relevant globals are declared in interface
                                                                     ;default values are in startup function


  let _n one-of (range 0 1 probDrought)                              ;MCH creates local variable _n, which is used to implement the probability of drought
                                                                     ;float [0,1]

  let _maxGrowthrateGrass maxGrowthrateGrass                         ;MCH creates local variable _maxGrowthrateGrass to store original maxGrowthrateGrass value
  if _n = 1 [set maxGrowthrateGrass maxGrowthrateGrass - sevDrought] ;MCH if _n = 1, sets the value of maxGrowthrateGrass for current tick to
                                                                     ;maxGrowthrateGrass - sevDrought

  set maxGrowthrateGrass _maxGrowthrateGrass                         ;MCH sets maxGrowthrateGrass value back to maxGrowthrateGrass value in previous period
end

to regrowGrass
  let _Veg (sum [grass] of patches)
  if (_Veg != count patches with [pcolor = green]) [error "change from Schindler model"] ;S.A.D. If local variable _Veg is not equal to the count of patches colored green, then run an error message
  let _maxV count patches
  let _nGrow round (_Veg * (maxGrowthrateGrass * _Veg * (1 - (_Veg / _maxV))))
  if (_nGrow != round (_Veg * (1 + (maxGrowthrateGrass * _Veg * (1 - (_Veg / _maxV)))) - _Veg)) [error "bad refactor"] ;MB - acts as an immeadiate test within function
  let _cangrow (patches with [grass < 1]) ;S.A.D. Let local variable _cangrow be patches with grass less than one
  ask (up-to-n-of _nGrow _cangrow) [set grass 1]
end

to graze  ;cow proc
  set forage 0
  while [forage < cowForageRequirement] [
    let _pcandidates (patches with [grass > 0])
    ifelse any? _pcandidates [
      move-to min-one-of _pcandidates [distance myself]
      set forage (forage + grass)
      set grass 0
    ][
      ; death will break out of the while loop
      ;    that a cow is running
      die
    ]
  ]
end
;this just provides a warning on zero reward

to-report identity [#x]
  print "encountered zero reward (selfishness must be 0)"
  report #x
end


; the `learn` proc will update `dHerd` based on reward
to learn  ;herder proc
  let _reward reward  ; compute the reward, all considerations included
  if (dHerd = 1) [
    set pAddCow (ifelse-value
          (_reward > 0) [pAddCow + (1 - pAddCow) * herderLearningFactor]
          (_reward < 0) [pAddCow * (1 - herderLearningFactor)]
                              [identity pAddCow])
  ]
  if (dHerd = -1) [
    set pAddCow (ifelse-value
          (_reward > 0) [pAddCow * (1 - herderLearningFactor)]
          (_reward < 0) [pAddCow + herderLearningFactor * (1 - pAddCow)]
                              [identity pAddCow])
  ]
  set dHerd ifelse-value (random-float 1.0 < pAddCow) [1] [-1]
end


to update-stock ;herder proc
  ;note: This forces herd size *always* to change.
  ifelse (dHerd = 1) [
    hatch-cows 1 [create-owner-from myself [hide-link]]
  ][
    if (dHerd != -1) [error "bad accounting"]
    ; up-to handles case of no cows
    ask (up-to-n-of 1 owner-neighbors) [die]
  ]
end


to-report cost [
  #initial-cows
  #after-cows
  ]
  let _maxGrass count patches ; to create a local varibale called _maxGrass equal to the count of all patches NB
  let _nGreen (sum [grass] of patches) ; to create a local varibale called _nGreen equal to the sum of grass of patches NB
  let _Veg1 (_nGreen - #initial-cows * cowForageRequirement) ; to create a local varibale called _Veg1 equal to _nGreen minus the initial # of cows multiplied by the cowforageReq NB
  let _grass-initial-cows (max list 0 _Veg1) * (1 + (maxGrowthrateGrass * _Veg1 * (1 - (_Veg1 / _maxGrass))))
  let _Veg2 (_nGreen - #after-cows * cowForageRequirement)
  let _grass-after-cows (max list 0 _Veg2) * (1 + (maxGrowthrateGrass * _Veg2 * (1 - (_Veg2 / _maxGrass))))
  report (_grass-initial-cows - _grass-after-cows) / (cowForageRequirement * count herders) * cowPrice
end

to test-Cost ;MB - Term Code.
  ;I wanted to revise the previous test attempt
  type "Begin testing `Cost` ... "
  let _iCows (count cows)
  ask herders [set dHerd 5] ;setting dHerd to a different value will better test of the cost function since its outside the norm.
  let _ACows (_iCows + sum [dHerd] of herders) ;taken from reward-single/reward-coop.
  let _cost cost _iCows _ACows ;answer 176.80178145086734
  let final round (_cost) ;177 - rounding gives the ability to say 'does not equal'
  if (final != 177) [error "Bad cost function"]
  print " test passes."
end

to-report myCows ; herder proc
  report owner-neighbors
end


to-report reward
  report selfishness * reward-single
         + cooperativeness * reward-coop
         + fairness-to-myself * reward-fairme
         + fairness-to-others * reward-fairother
         + positive-reciprocity * reward-posrec
         + negative-reciprocity * reward-negrec
         + conformity * reward-conf
         + risk-aversion * reward-risk
end


to-report reward-single ; herder proc
  let _K (count cows) ;MB - creates local variable _K = the number of cows.
  ;MB - Only used within this function, which is why in order to use it again it must be recreated as another local variable. Seen in reward-coop
  let _cost cost (_K + sum [dHerd] of other herders) (_K + sum [dHerd] of herders)
  let _fin-reward-single (dHerd * cowPrice - _cost)
  report ifelse-value (_fin-reward-single > 0) [1] [-1]
end


to-report reward-coop ; herder proc
  let _K (count cows)
  let _cost cost _K (_K + sum [dHerd] of herders)
  let _fin-reward-coop (sum [dHerd] of herders * cowPrice / count herders - _cost)
  report ifelse-value (_fin-reward-coop > 0) [1] [-1]
end

to-report reward-fairme ; herder proc
  let _nOthers (count herders - 1) ;3
  let _Ko (count cows - count owner-neighbors) ;120 -30 = 110
  let _otherAv (_Ko / _nOthers) ;110/3
  report ifelse-value (_otherAv > count myCows) [dHerd] [0]
end


to-report reward-fairother ; herder proc
  ;MB - calculating liklihood of agents avioding actions that make that agent better off
  let _nOthers (count herders - 1)
  let _Ko (count cows - count owner-neighbors)
  let _otherAv (_Ko / _nOthers)
  report ifelse-value (_otherAv < count myCows) [(- dHerd)] [0]
end
; reward to "positive reciprocity" (i.e., matching others’ herd *decreases*)


to-report reward-posrec ; herder proc
  ;MB - liklihood of favoring/doing actions that reward other agents
  let _meanChangeOthers (mean [dHerd] of other herders)
  report ifelse-value (_meanChangeOthers < 0) [(- dHerd)] [0]
end
; reward to "negative reciprocity" (i.e., matching others’ herd *increases*)


to-report reward-negrec ; herder proc
  ;MB - liklihood of favoring/doing actions that harm other agents
  let _meanChangeOthers (mean [dHerd] of other herders)
  report ifelse-value (_meanChangeOthers > 0) [dHerd] [0]
end


to-report sign [#x]
  report (ifelse-value
        (#x < 0) [-1]
        (#x > 0) [1]
                 [0])
end
; reward to conformity


to-report reward-conf ; herder proc
  ;MB - liklihood of favoring behavior that is close to the past behavior of the group
  let _meanChangeOthers (mean [dHerd] of other herders)
  report (sign _meanChangeOthers) * dHerd
end


to-report reward-risk ; herder proc
  ;MB - liklihood of favoring actions that reduce the agents risk
  let _K (count cows)
  let _cost (cost _K (_K + count herders))
  report ifelse-value (_cost > cowPrice) [(- dHerd)] [0]
end


to updateGUI
  ask patches [set pcolor ifelse-value (grass > 0) [green] [brown]] ; to ask patches to set color to green if grass equal 1 if grass equal 0 set color to brown NB
  ask cows [set color white set size 2] ; ask cows to have the color white and set the size to 2 NB
  ;rewardVisual ;MB Added here
  ;initialize plots:
  set-current-plot "Grass Cover" ; to plot the grass cover shows a decreasing plot NB
  let _cover 100 * (sum [grass] of patches) / (count patches)
  plot _cover
  set-current-plot "Number of Cows" ; to plot the numbers of cows shows a decreasing plot NB
  plot count cows
end

;;;TESTS;;;
to test-sharesteakwithFriends ;S.A.D.
  print "Testing sharesteakwithFriends procedure..."
  let _count1 count cows ;create a local variable with the count of cows
  sharesteakwithFriends ;run procedure
  let _count2 count cows ;create a second local variable with the count of cows from AFTER the procedure has been run
  ifelse (any? herders with [count my-friendships > 0]) [ ;ask the herders with friends to:
    if (_count1 > _count2) [print "The procedure passed: sharesteakwithFriends had an effect"]] [ ;compare the counts of cows and if the second list is smaller, print phrase
  print "No need to test sharesteakwithFriends because you can't use it on anything."] ;otherwise print this
end

to test-seefriends ;Michael made this
  ;seefriendshipsandMove ;only need this if it hasn't ran already

  ask herders [                                    ;MCH test if the herders have moved distance defined by seefriendshipsandMove
    ifelse (count herders with [color = gray] in-radius 10 > 1)
    [print "bad move, some herders still too close"]
    [print "good move, herders no longer close together"]
  ]

  ifelse (count herders with [color = gray - 1] = 0) ;MCH test if herder colors have reverted to gray
    [print "good reversion for herder color"]
    [print "bad color for herder"]

  ifelse (count patches with [pcolor = green] = count patches with [grass > 0]) ;MCH test if patches with grass have reverted color to green
    [print "good reversion for patches with grass"]
    [print "bad reversion of grass"]

  ifelse (count patches with [pcolor = brown] = count patches with [grass = 0]) ;MCH test if patches with no grass have reverted color to brown
    [print "good reversion for patches with no grass"]
    [print "bad reversion of dirt"]
end

to test-RecalculateGrows ; test for proc NB
  show "Begin test of RecalculateGrows ... "
  setup
  let tmp pctGrassInitial
  set pctGrassInitial 50
  set weather "normal"
  show "weather is normal, checking number of grass"
  ; the new number of patches should be
  ; define main variables to check growth number
  let _Veg (sum [grass] of patches)
  let _nGrassPatchesExpected round (pctGrassInitial / 100 * count patches)
  let _maxV count patches
  let _nGrow round (_Veg * (maxGrowthrateGrass * _Veg * (1 - (_Veg / _maxV))))
  let _cangrow count (patches with [grass < 1])
  ; launch reGrow procedure (and weather influence including)
  regrowGrass
  ; check how it affect number of new green patches
  ifelse _cangrow <= _nGrow [
    ifelse (sum [grass] of patches - _cangrow -  _Veg) = 0 [show "For normal weather growth number is good!"][
      show "For normal weather growth number incorrect"
    ]
  ][
    ifelse (sum [grass] of patches - _nGrow -  _Veg) = 0 [show "For normal weather growth number is good!"][
      show "For normal weather growth number incorrect"
    ]

  ]

  ; for rainy weather
  setup
  show "weather is raining, checking number of grass"
  set weather "raining"
  set _Veg (sum [grass] of patches)
  set _nGrow round (_Veg * (maxGrowthrateGrass * _Veg * (1 - (_Veg / _maxV))))
  set _cangrow count (patches with [grass < 1])
  ; weather influence
  set _nGrow _nGrow * 2
    ; launch reGrow procedure (and weather influence including)
  regrowGrass
  ; check how it affect number of new green patches
  ifelse _cangrow <= _nGrow [
    ifelse (sum [grass] of patches - _cangrow -  _Veg) = 0 [show "For raining growth number is good!"][
      show "For raining growth number incorrect"
    ]
  ][
    ifelse (sum [grass] of patches - _nGrow -  _Veg) = 0 [show "For raining growth number is good!"][
      show "For raining growth number incorrect"
    ]
  ]

  ; for dry weather

  setup
  show "weather is dry, checking number of grass"
  set weather "dry"
   set _Veg (sum [grass] of patches)
  set _nGrow round (_Veg * (maxGrowthrateGrass * _Veg * (1 - (_Veg / _maxV))))
  set _cangrow count (patches with [grass < 1])
  ; weather influence
  set _nGrow _nGrow / 2
  ; launch reGrow procedure (and weather influence including)
  regrowGrass
  ; check how it affect number of new green patches
  ifelse _cangrow <= _nGrow [
    ifelse (sum [grass] of patches - _cangrow -  _Veg) = 0 [show "For dry growth number is good!"][
      show "For dry growth number incorrect"
    ]
  ][
    ifelse (sum [grass] of patches - _nGrow -  _Veg) = 0 [show "For dry growth number is good!"][
      show "For dry growth number incorrect"
    ]
  ]
end

to test-startup ;S.A.D.
  type "Begin test of startup..."
  if (4 != nHerdersInitial) [error "Wrong number of initial herders. Check it."]
  if (120 != nCowsInitial) [error "Wrong number of initial cows. Double check the herds."]
  if (100 != pctGrassInitial) [error "Wrong amount of grass, sorry."]
  if (0.0010 != maxGrowthrateGrass) [error "Check your grass growth rate, something is wrong with it."]
  if (1 != cowForageRequirement) [error "Check your cow forge requirement, something is off."]
  ;ai: hmm v aren't you using a Hardin baseline
  if (1 != selfishness) [error "Check your selfishness baseline."]
  if (0 != cooperativeness) [error "Check your cooperativeness baseline."]
  if (0 != fairness-to-myself) [error "Check your fairness-to-myself baseline."]
  if (0 != fairness-to-others) [error "Check your fairness-to-others baseline."]
  if (0 != negative-reciprocity) [error "Check your negative-reciprocity baseline."]
  if (0 != conformity) [error "Check your conformity baseline."]
  if (0 != risk-aversion) [error "Check your risk-aversion baseline."]
  print "startup is ok"
end

;ai: Testing the cost reporter is a good idea but this does not do that.
;to test-cost ;MB
;  type "Begin test of cost..."
;  let _maxGrass count patches
;  ;ai: compare this ^ and this v; it's really not a test
;  if (count patches != _maxGrass) [error "bad count"]
;  ;ai: well v OK, ... but create a real test
;  if (not is-number? _maxGrass) [error "_maxGrass should be a number"]
;  let _nGreen (sum [grass] of patches)
;  ;ai: this ^ will only work if green is a number;
;  ;    still, this is almost a test of `setup`
;  if (not is-number? _nGreen) [error "_nGreen should be a number"]
;  print "Cost Okay!"
;end

;ai: see test-setup02 (below)
to test-setup   ;MCH
  ;ai: set up before testing any of the setup
  let _nPatches round (pctGrassInitial / 100 * count patches)                                                      ;code from Prof. Alan Isaac
  ;ai: the following equality is expecte but you treat it as bad
  ifelse _nPatches = round (pctGrassInitial / 100 * count patches) [print "good nPatches"] [print "bad_nPatches"]  ;testing variable setup,
                                                                                                                   ;intended to call setup and then run this line, but
                                                                                                                   ;results in variable _nPatches not being found
  setup
  ;ai: use error, not print
  ifelse (count patches with [grass = 1] = 0) [print "error, no patches have grass"] [print "some patches have grass"]       ;testing setting up patches with grass
  ;ai: one-of makes a random choice; use member?
  ifelse (count herders with [dHerd = one-of [-1 1]] = herders) [print "good dHerd"] [print "bad dHerd"]                     ;testing if herders have dHerd with value of -1 or 1
  ifelse (count herders with [pAddCow = 0.5] = herders) [print "good pAddCow for herders"] [print "bad pAddCow for herders"] ;testing if pAddCow has been set for the herders
;  ifelse (count cows with [herder = owner] = cows) [print "all cows have herder as owner"] [print "some or all cows lack herder as owner"]
  ;not sure how to test if cows have owner
  ifelse (ticks = 1) [print "ticks have been reset"] [print "bad ticks"]  ;check if reset-ticks has been called
  ;not sure how to check if setupGUI has been run
end

;ai: for illustration
to test-setup02
  type "Begin test of `setup` ... "
  setup
  if (0 != ticks) [error "bad ticks value at end of setup"]
  let _nGrassPatchesExpected round (pctGrassInitial / 100 * count patches)
  if (count patches with [1 = grass] != _nGrassPatchesExpected) [error "bad grass setup"]
  if (count patches with [1 = grass] != count patches with [green = pcolor]) [error "bad GUI setup"]
  if (count herders != nHerdersInitial) [error "bad herder setup"]
  if (count herders with [member? dHerd [-1 1]] != nHerdersInitial) [error "bad herder setup"]
  if (count herders with [0.5 = pAddCow] != nHerdersInitial) [error "bad herder setup"]
  print " test passes."
end
@#$#@#$#@
GRAPHICS-WINDOW
243
15
614
387
-1
-1
11.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
5
10
225
43
nHerdersInitial
nHerdersInitial
0
25
4.0
1
1
NIL
HORIZONTAL

SLIDER
5
52
225
85
nCowsInitial
nCowsInitial
0
1000
120.0
1
1
NIL
HORIZONTAL

SLIDER
5
95
225
128
cowPrice
cowPrice
1
200
92.0
1
1
$
HORIZONTAL

SLIDER
5
135
225
168
pctGrassInitial
pctGrassInitial
0
100
100.0
1
1
%
HORIZONTAL

TEXTBOX
5
180
225
205
Grass growth rate (maximum):
11
0.0
1

SLIDER
5
200
225
233
maxGrowthrateGrass
maxGrowthrateGrass
0
0.01
0.001
0.001
1
NIL
HORIZONTAL

TEXTBOX
5
240
225
271
Patches browsed per cow per time step:
11
0.0
1

SLIDER
5
260
225
293
cowForageRequirement
cowForageRequirement
1
66
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
5
307
171
325
Rate of adaptation to rewards:
11
0.0
1

SLIDER
5
324
222
357
herderLearningFactor
herderLearningFactor
0
1
0.74
0.01
1
NIL
HORIZONTAL

SLIDER
632
85
804
118
cooperativeness
cooperativeness
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
632
48
804
81
selfishness
selfishness
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
632
122
804
155
fairness-to-myself
fairness-to-myself
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
633
159
805
192
fairness-to-others
fairness-to-others
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
633
196
806
229
positive-reciprocity
positive-reciprocity
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
634
232
807
265
negative-reciprocity
negative-reciprocity
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
634
269
807
302
conformity
conformity
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
634
306
808
339
risk-aversion
risk-aversion
0
1
0.0
0.01
1
NIL
HORIZONTAL

BUTTON
5
365
115
424
Set Up
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
120
365
219
423
Go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
635
350
808
383
Restore Hardin Settings
hardin-settings
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
633
14
792
42
Socio-psychological dispositions\n(default is Hardin's scenario)
11
0.0
1

PLOT
829
10
1098
200
Grass Cover
NIL
NIL
0.0
10.0
50.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
826
205
1101
394
Number of Cows
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

BUTTON
12
435
285
468
Try to move friends that are too close
seefriendshipsandMove
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
356
399
526
432
Show rewards visually 
rewardVisual
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
646
419
818
452
probDrought
probDrought
0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
649
464
821
497
sevDrought
sevDrought
0
0.01
0.005
0.0001
1
NIL
HORIZONTAL

BUTTON
838
427
1202
460
Start Drought -- Need to Uncomment in Step Function
drought
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
13
475
194
508
Have a feast with friends
sharesteakwithFriends
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

CHOOSER
365
449
503
494
weather
weather
"raining" "dry" "normal"
0

@#$#@#$#@
## WHAT IS IT?

## HOW IT WORKS

## HOW TO USE IT

## THINGS TO NOTICE

## THINGS TO TRY

## EXTENDING THE MODEL

## NETLOGO FEATURES

## RELATED MODELS

## CREDITS AND REFERENCES

This implementation is copyright (c) 2020 by Alan G. Isaac. Since this implementation is derivative of the Schindler (2013) model at https://www.openabm.org/model/3051, it too is licensed under the GPL 2.0.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="MB_Initial Experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count cows</metric>
    <metric>count patches with [pcolor = green]</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SAD_Initial_Experiment" repetitions="20" runMetricsEveryStep="true">
    <setup>setup
randomize</setup>
    <go>go</go>
    <timeLimit steps="365"/>
    <metric>count cows</metric>
    <metric>count patches with [pcolor = green]</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.4"/>
      <value value="0.58"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0.92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="0.62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0.68"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MCH_Initial_Experiment_LowRiskAversion" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.58"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="102"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0.92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="27"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0.68"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="94"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MCH_Initial_Experiment_LowRS_HighLearning" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.58"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.008"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="102"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0.92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="27"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0.68"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="94"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MCH_Experiment2" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count cows</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="selfishness" first="0" step="5" last="20"/>
    <enumeratedValueSet variable="conformity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="positive-reciprocity" first="0" step="5" last="20"/>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SAD_Experiment_2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup
randomize</setup>
    <go>go</go>
    <timeLimit steps="365"/>
    <metric>count cows</metric>
    <metric>count patches with [pcolor = green]</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.4"/>
      <value value="0.58"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0.5"/>
      <value value="0.68"/>
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0.92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="0.62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MCH_Term_Project_Experimental_Design" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count cows</metric>
    <metric>count patches with [grass = 1]</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fairness-to-others" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fairness-to-myself" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MB_Term_Experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count cows</metric>
    <metric>count patches with [pcolor = green]</metric>
    <metric>myCows</metric>
    <metric>reward</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0.68"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MB_TERM_CODE" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count cows</metric>
    <metric>myCows</metric>
    <metric>patches with [pcolor = green]</metric>
    <metric>reward</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0.68"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0.78"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="MCH_Term_Project_Code" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50"/>
    <metric>count cows</metric>
    <metric>count patches with [grass = 1]</metric>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sevDrought">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probDrought">
      <value value="0.05"/>
      <value value="0.01"/>
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SAD_Term_Project_Code" repetitions="20" runMetricsEveryStep="true">
    <setup>setup
seefriendshipsandMove
sharesteakwithFriends</setup>
    <go>go</go>
    <timeLimit steps="365"/>
    <metric>count cows</metric>
    <metric>count patches with [pcolor = green]</metric>
    <enumeratedValueSet variable="positive-reciprocity">
      <value value="0.5"/>
      <value value="0.68"/>
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-others">
      <value value="0.4"/>
      <value value="0.58"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="0.8"/>
      <value value="0.75"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herderLearningFactor">
      <value value="0.74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maxGrowthrateGrass">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nCowsInitial">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="negative-reciprocity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nHerdersInitial">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cooperativeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowPrice">
      <value value="92"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cowForageRequirement">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selfishness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="conformity">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sevDrought">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fairness-to-myself">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pctGrassInitial">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probDrought">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
