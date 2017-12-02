%Andrew Ang
%Prof. Arias
%CSC 3310 Concepts of Programming Languages
%Prolog HW

%A point that takes in an x-coordinate and a y-coordinate
point2d(12,2).
point2d(4, 13).
point2d(20 ,1).

%Segment two points
segment(point2d(5, 4), point2d(5, 5)).
segment(point2d(4, 12), point2d(6, 10)).

rectangle(point2d(9, 16), point2d(16, 14)). %Upper Left and Lower Right corners
rectangle(point2d(3, 6), point2d(10, 3)). %Upper Left and Lower Right corners

square(point2d(3, 13), 4). %Upper Left corner and length of side
square(point2d(11, 6), 2). %Upper Left corner and length of side

circle(point2d(12, 4), 3). %Center, radius

%Helper rule that determines the slope
slope(S,segment(point2d(X, Y), point2d(X2, Y2))) :-
	(S is (Y2 - Y) / (X2 - X)).

%Helper rule, 1st is line 2nd is radius, Distance formula
% it "returns" Distance (D)
distance(D, X, Y, X2, Y2) :-
	D is sqrt((X - X2) * (X - X2) + (Y - Y2) * (Y - Y2)).

%Checks if the line is verticle
vertical(segment(point2d(X,Y), point2d(X2, Y2))) :-
	X =:= X2.

%Checks if the line is horizontal
horizontal(segment(point2d(X, Y), point2d(X2, Y2))) :-
	Y =:= Y2.

%checks if two lines are parallel to each other
parallel(segment(point2d(X,Y), point2d(X2,Y2)), 
	segment(point2d(X3,Y3), point2d(X4,Y4))) :-
		%if the denominator for the slope is zero just say true
		(	(X2 - X =:= 0) ->
			true
		; % else find the slope of both lines and make sure they equal
			(slope(S, segment(point2d(X,Y), point2d(X2,Y2))), 
			slope(S2, segment(point2d(X3,Y3), point2d(X4,Y4))),
			S =:= S2)	
		).

%Line is perpendicular to another line
perpendicular(segment(point2d(X,Y), point2d(X2,Y2)), 
	segment(point2d(X3,Y3), point2d(X4,Y4))) :-
		slope(S, segment(point2d(X,Y), point2d(X2,Y2))), 
		slope(S2, segment(point2d(X3,Y3), point2d(X4,Y4))),
		(S * S2) =:= -1.

% check if line is inside circle
contained(segment(point2d(X,Y), point2d(X2,Y2)), 
	circle(point2d(X3, Y3), R)) :-
		distance(D, X, Y, X3, Y3), 
		distance(D2, X2, Y2, X3, Y3),
		D < R,
		D2 < R.

% check if line is inside a square
contained(segment(point2d(X,Y), point2d(X2,Y2)), 
	 square(point2d(X3, Y3), L)) :- 
	 	X > X3,
	 	X < X3 + L,
	 	Y < Y3,
	 	Y > Y3 - L,
	 	X2 > X3,
	 	X2 < X3 + L,
	 	Y2 < Y3,
	 	Y2 > Y3 - L.

% check if line is inside a rectangle
contained(segment(point2d(X,Y), point2d(X2,Y2)),
	rectangle(point2d(X3, Y3),point2d(X4, Y4))) :-
		X > X3,
		X < X4,
		Y > Y4,
		Y < Y3,
		X2 > X3,
		X2 < X4,
		Y2 > Y4,
		Y2 < Y3.


%square containted inside circleL is length and R is radius 
contained(square(point2d(X, Y), L), circle(point2d(X2, Y2), R)) :-
		distance(D, X, Y, X2, Y2), 
		distance(D2, (X + L), Y, X2, Y2),
		distance(D3, X, (Y - L), X2, Y2), 
		distance(D4, (X + L), (Y - L), X2, Y2),
		D < R,
		D2 < R,
		D3 < R,
		D4 < R.

%circle containted inside a square L is length and R is radius
contained(circle(point2d(X2, Y2), R), square(point2d(X, Y), L)) :-
	distance(D, X, Y, X2, Y2), 
	distance(D2, (X + L), Y, X2, Y2),
	distance(D3, X, (Y - L), X2, Y2), 
	distance(D4, (X + L), (Y - L), X2, Y2),
	D > R,
	D2 > R,
	D3 > R,
	D4 > R.

% rectangle inside circle, R is radius 
contained(rectangle(point2d(X, Y),point2d(X2, Y2)), 
	circle(point2d(X3, Y3), R)) :-
	distance(D, X, Y, X3, Y3), 
	distance(D2, X2, Y2, X3, Y3),
	distance(D3, X, Y2, X3, Y3), 
	distance(D4, X2 , Y, X3, Y3),
	D < R,
	D2 < R,
	D3 < R,
	D4 < R.

% circle inside a rectangle, R = radius
contained(circle(point2d(X3, Y3), R),
	rectangle(point2d(X, Y),point2d(X2, Y2))) :-
		distance(D, X, Y, X3, Y3), 
		distance(D2, X2, Y2, X3, Y3),
		distance(D3, X, Y2, X3, Y3), 
		distance(D4, X2 , Y, X3, Y3),
		D > R,
		D2 > R,
		D3 > R,
		D4 > R.

% square inside a rectangle, L is length
contained(square(point2d(X, Y), L),
	rectangle(point2d(X2, Y2),point2d(X3, Y3))) :-
		X > X2 , X < X3,
		Y > Y3 , Y < Y2,
		(X + L) > X2, (X + L) < X3,
		(Y - L) > Y3, (Y - L) < Y2.

% rectangle inside a square
contained(rectangle(point2d(X2, Y2),point2d(X3, Y3)),
	square(point2d(X, Y), L)) :-
		X2 > (X - L), X2 < X,
		Y2 > Y, Y2 < (Y + L),
		X3 > (X - L), X3 < X,
		Y3 > Y, Y3 < (Y + L).

%Check if a point is in a circle, R is radius
in(point2d(X,Y), circle(point2d(X2, Y2), R)) :-
	distance(D, X, Y, X2, Y2), 
	D < R.

%Check if a point is in a square L is length
in(point2d(X,Y), square(point2d(X2, Y2), L)) :-
	 X > X2,
	 X < X2 + L,
	 Y < Y2,
	 Y > Y2 - L.

%Check if a point is in a rectangle
in(point2d(X,Y), rectangle(point2d(X3, Y3),point2d(X4, Y4))) :-
	X > X3,
	X < X4,
	Y > Y4,
	Y < Y3.

%Finds the y-intercept given the slope (M) and X and Y coordinate
% "returns" the y-intercept
findB(B, M, X, Y) :-
	B is Y - (M * X).

% checks if the point is on a line
on(point2d(X,Y), segment(point2d(X2, Y2), point2d(X3, Y3))) :-
			%if both the the denominator is zero it is a vertical line
		(	(
				(X3 - X2 =:= 0),  %if
				X =:= X2,
				Y =< Y2,
				Y >= Y3
			) 
			-> 
			true
			
		;
			(
				(X3 - X2 =:= 0), 
				X =:= X2,
				Y =< Y3,
				Y >= Y2
			)
			->
			true		
		 	% if the denominator equals 0 but not on the segment			
		; (X3 - X2 =:= 0) ->
			false
		;
		% else complete the equation y=mx+b for given point
		slope(S,segment(point2d(X2, Y2), point2d(X3, Y3))),
		findB(B, S, X2, Y2),
		Y =:= S * X + B
		).

%checks if point is on a circle
on(point2d(X,Y), circle(point2d(X2, Y2), R)) :-
	distance(D, X, Y, X2, Y2), 
	D =:= R.

%checks if a point is on a square
on(point2d(X,Y), square(point2d(X2, Y2), L)) :-
	(X =:= X2, Y =< Y2, Y >= Y2 - L);
	(Y =:= Y2 - L, X =< X2 + L, X >= X2);
	(X =:= X2 + L, Y =< Y2, Y >= Y2 - L);
	(Y =:= Y2, X =< X2 + L, X >= X2).

%checks if a point is on a rectangle
on(point2d(X,Y), rectangle(point2d(X2, Y2),point2d(X3, Y3))) :-
	(X =:= X2, Y =< Y2, Y >= Y3);
	(Y =:= Y2, X =< X3, X >= X2);
	(X =:= X3, Y =< Y2, Y >= Y3);
	(Y =:= Y3, X =< X3, X >= X2).

%finds the width of a 4 sided polygon
% "returns" the width of the rectangle/square
findWidth(W, X, X2) :-
	W is X2 - X.

%finds the height of a 4 sided polygon
% "returns" the height of a rectangle/square
findHeight(H, Y, Y2) :-
	H is Y - Y2.

%finds the distance between the circles X center and rect X center
% "returns" the distance (X value)
findDX(DX, X, X2, X3) :-
	findWidth(W, X2, X3),
	DX is abs(X - W/2).

%finds the distance between the circles Y center and rect Y center
% "returns" the distance (Y value)
findDY(DY, Y, Y2, Y3) :-
	findHeight(H, Y2, Y3),
	DY is abs(Y - H/2).

% square intersects rectangle
% checks if there is one point in rest out
% also checks if there is two points in rest out
intersects(square(point2d(X, Y), L), 
	rectangle(point2d(X2, Y2),point2d(X3, Y3))) :-
		
		%make sure that they are not contained within each other
		not(
			contained(square(point2d(X, Y), L),
			rectangle(point2d(X2, Y2),point2d(X3, Y3)))
		),
		not(
			contained(rectangle(point2d(X2, Y2),point2d(X3, Y3)),
			square(point2d(X, Y), L))
		),

	(
		%top left in rest out just one point
		(
		in(point2d(X2,Y2), square(point2d(X, Y), L)),
		not(in(point2d(X2,Y3), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L)))
		);

		%top right in rest out just one point
		(
		in(point2d(X3,Y2), square(point2d(X, Y), L)),
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X2,Y3), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L)))
		);

		%bottom left in rest out just one point
		(
		in(point2d(X2,Y3), square(point2d(X, Y), L)),
		not(in(point2d(X3,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L)))
		);

		%bottom right in rest out just one point
		(
		in(point2d(X3, Y3), square(point2d(X, Y), L)),
		not(in(point2d(X3,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X2,Y3), square(point2d(X, Y), L)))
		);

		%two points in top left and bottom left
		(
		in(point2d(X2,Y2), square(point2d(X, Y), L)),
		in(point2d(X2,Y3), square(point2d(X, Y), L)),
		not(in(point2d(X3,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L)))
		);

		%two points in bottom left and bottom right
		(
		in(point2d(X2,Y3), square(point2d(X, Y), L)),
		in(point2d(X3,Y3), square(point2d(X, Y), L)),
		not(in(point2d(X3,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X2,Y2), square(point2d(X, Y), L)))
		);

		%two points in bottom right and top right
		(
		in(point2d(X3,Y3), square(point2d(X, Y), L)),
		in(point2d(X3,Y2), square(point2d(X, Y), L)),
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X2,Y3), square(point2d(X, Y), L)))
		);

		%two points in top left and top right
		(
		in(point2d(X2,Y2), square(point2d(X, Y), L)),
		in(point2d(X3,Y2), square(point2d(X, Y), L)),
		not(in(point2d(X2,Y3), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L)))
		)
	).

% rectangle intersects square
intersects(rectangle(point2d(X2, Y2),point2d(X3, Y3)),
	square(point2d(X, Y), L)) :-
		intersects(square(point2d(X, Y), L), 
		rectangle(point2d(X2, Y2),point2d(X3, Y3))).

%circle intersects a rectangle
intersects(circle(point2d(X, Y), R),
	rectangle(point2d(X2, Y2),point2d(X3, Y3))) :-
		%make sure that they are not contained within each other
		not(contained(rectangle(point2d(X2, Y2),
			point2d(X3, Y3)), 
			circle(point2d(X, Y), R))
    	),
		not(contained(circle(point2d(X, Y), R),
			rectangle(point2d(X2, Y2),point2d(X3, Y3)))
		),
		findWidth(W, X2, X3),
		findHeight(H, Y2, Y3),
		findDX(DX, X, X2, X3),
		findDY(DY, Y, Y2, Y3),
		DX =< W/2,
		DY =< H/2,
		DISTX is DX - W/2,
		DISTY is DY - H/2,
		(DISTX * DISTX + DISTY * DISTY) =< R * R.

%rectangle intersects a circle
intersects(rectangle(point2d(X2, Y2),point2d(X3, Y3)),
	circle(point2d(X, Y), R)) :-
		intersects(circle(point2d(X, Y), R),
		rectangle(point2d(X2, Y2),point2d(X3, Y3))).

%circle intersects a square
intersects(circle(point2d(X, Y), R),
	square(point2d(X2, Y2), L)) :-
		%make sure that they are not contained within each other
		not(
			contained(circle(point2d(X2, Y2), R), 
			square(point2d(X, Y), L))
		),
		not(
			contained(square(point2d(X, Y), L), 
			circle(point2d(X2, Y2), R))
		),
		findWidth(W, X2, X2 + L),
		findHeight(H, Y2, Y2 - L),
		findDX(DX, X, X2, X2 + L),
		findDY(DY, Y, Y2, Y2 - L),
		DX =< W/2,
		DY =< H/2,
		DISTX is DX - W/2,
		DISTY is DY - H/2,
		(DISTX * DISTX + DISTY * DISTY) =< R * R.

%square intersects a circle
intersects(square(point2d(X2, Y2), L),
	circle(point2d(X, Y), R)) :-
		intersects(circle(point2d(X, Y), R),
		square(point2d(X2, Y2), L)).

%a circle intersects a segment
intersects(circle(point2d(X,Y),R), 
	segment(point2d(X2,Y2), point2d(X3,Y3))) :-
		( % first point is in, 2nd out
			in(point2d(X2,Y2), circle(point2d(X, Y), R)),
			not(in(point2d(X3,Y3), circle(point2d(X, Y), R)))
		);
		( % first poitn is out 2nd in
			not(in(point2d(X2,Y2), circle(point2d(X, Y), R))),
			in(point2d(X3,Y3), circle(point2d(X, Y), R))
		);
		( %both points are out check the midpoint, 
			not(in(point2d(X3,Y3), circle(point2d(X, Y), R))),
			not(in(point2d(X2,Y2), circle(point2d(X, Y), R))),
			in(point2d(((X2 + X3) / 2), Y2), circle(point2d(X, Y), R))
		);
		(	%both points are out check the midpoint 
			not(in(point2d(X3,Y3), circle(point2d(X, Y), R))),
			not(in(point2d(X2,Y2), circle(point2d(X, Y), R))),
			in(point2d(X2, ((Y2 + Y3) / 2)), circle(point2d(X, Y), R))
		).

%a segment intersects a circle
intersects(segment(point2d(X2,Y2), point2d(X3,Y3)),
	circle(point2d(X,Y),R)) :-
		intersects(circle(point2d(X,Y),R), 
		segment(point2d(X2,Y2), point2d(X3,Y3))).

%Checks if there is a segment intersects a square
intersects(segment(point2d(X2,Y2), point2d(X3,Y3)),
	square(point2d(X, Y), L)) :-
	(
		in(point2d(X2,Y2), square(point2d(X, Y), L)),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L)))
	);
	(
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		in(point2d(X3,Y3), square(point2d(X, Y), L))
	);
	(
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L))),
		in(point2d(((X2 + X3) / 2),Y2), square(point2d(X, Y), L))
	);
	(
		not(in(point2d(X2,Y2), square(point2d(X, Y), L))),
		not(in(point2d(X3,Y3), square(point2d(X, Y), L))),
		in(point2d(X2, ((Y2 + Y3) /2)), square(point2d(X, Y), L))
		
	).

%Check if a square intersects a segment
intersects(square(point2d(X, Y), L), 
	segment(point2d(X2,Y2), point2d(X3,Y3))) :-
		intersects(segment(point2d(X2,Y2), point2d(X3,Y3)),
		square(point2d(X, Y), L)).

%Check if a rectangle intersects a segment
intersects(rectangle(point2d(X, Y),point2d(X2, Y2)), 
	segment(point2d(X3,Y3), point2d(X4,Y4))) :-
	( %in one point out another point
		in(point2d(X3,Y3), rectangle(point2d(X, Y),point2d(X2, Y2))),
		not(in(point2d(X4,Y4), rectangle(point2d(X, Y),point2d(X2, Y2))))
	);	
	( %in another point out other point
		not(in(point2d(X3,Y3), rectangle(point2d(X, Y),point2d(X2, Y2)))),
		in(point2d(X4,Y4), rectangle(point2d(X, Y),point2d(X2, Y2)))
	);
	( %check if midpoint is in
		not(in(point2d(X3,Y3), rectangle(point2d(X, Y),point2d(X2, Y2)))),
		not(in(point2d(X4,Y4), rectangle(point2d(X, Y),point2d(X2, Y2)))),
		in(point2d(((X3 + X4) / 2),Y3), rectangle(point2d(X, Y),point2d(X2, Y2)))
	);
	( %check if midpoint is in
		not(in(point2d(X3,Y3), rectangle(point2d(X, Y),point2d(X2, Y2)))),
		not(in(point2d(X4,Y4), rectangle(point2d(X, Y),point2d(X2, Y2)))),
		in(point2d(X3, ((Y3 + Y4) /2)), rectangle(point2d(X, Y),point2d(X2, Y2)))
	).

%Check if a rectangle intersects a segment
intersects(segment(point2d(X3,Y3), point2d(X4,Y4)),
	rectangle(point2d(X, Y),point2d(X2, Y2))) :-
		intersects(rectangle(point2d(X, Y),point2d(X2, Y2)), 
		segment(point2d(X3,Y3), point2d(X4,Y4))).
