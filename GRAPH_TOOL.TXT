#-----------------------------------------------------------------------
#                           G R A P H   T O O L
#-----------------------------------------------------------------------
#.m
!*******************************************************************************
 Image tool, facility=graph, short_name=gt
!-----------------------------------------
%ab 
    Graphing tool to display visually mathermatical function
    (c) 2001 B. Chiha
%end %ab
%keywords graph,tool

Interface

CLD_syntax
    define verb gt
    image "genbld:graph_tool.exe"
end CLD_syntax

end interface
Implementation

Shared

%include 'utebld:smg_stuff.bas'

Record screen_ids
    Long pboard    ! Pasteboard
    Long main      ! Main Screen
    Long menu      ! Menu Screen
    Long popup     ! Popup Screen
    Long formula   ! formula Screen
    Long help      ! Help Screen
    Long title     ! Title Screen
    Long Kboard    ! Keyboard
End Record

!Menu Enumerator Type
enum_type menu_opt
    attributes
         string label = 10   !label
    end attributes
	bounds=1 ("Set Bounds")
	formula	 ("Formula")
	execute	 ("Execute")
	help	 ("Help")
	exit	 ("Exit")
end enum_type

!Formula Type Enumerator Type
enum_type formula_typ
    attributes
         string formula = 15   !Formula
    end attributes
	sin=1    ("y = 10 * SIN(x)")
	cos	 ("y = 10 * COS(x)")
	tan	 ("y = 10 * TAN(X)")
	log	 ("y = LOG(X)")
	paro	 ("y = X^2")
end enum_type

!Map Coordinates
declare long Constant   gw_height = 21, &
                        gw_length = 68, &
                        menu_length = 12, &
                        exit_option = 3, &
                        upperbd = 1, &
                        lowerbd = 0

!Screen Titles
declare string Constant graph_title = "SIS GRAPHING TOOL", &
                        main_win = "Graph", &
                        help_win = "SIS Graphing Tool Help", &
                        formula_win = "Select Formula"

!Special Characters
declare string Constant point_char = '*'

!Global Variables -- Yuk
map (gt_data) screen_ids scrn

End Shared
!_______________________________________________________________________________
Program graph_tool
option constant type=Integer, handle=severe

! Create Pasteboard and virtual displays
! --------------------------------------
call gt.init_screen

! Initiate Main Menu Loop
! -----------------------

declare word selection, xbound(upperbd), ybound(upperbd), xinc, yinc
declare formula_typ. curr_formula

while selection <> graph_menu_opt_exit 
    call smg$select_from_menu(scrn::kboard, scrn::menu, selection, &
                        !default_choice!, &
                        !flags!, &
                        !help lib!, &
                        !time_out in sec!, &
                        !terminator!, &
                        !selected_item!, &
                        !rend!smg$m_bold, &
                        !rend-com!) 
    select selection
        case graph_menu_opt_bounds
            call gt.get_bounds(xbound(),ybound(),xinc,yinc)
        case graph_menu_opt_formula
            call gt.get_formula(curr_formula)
        case graph_menu_opt_execute
            call gt.execute_graph(curr_formula, xbound(), ybound(), xinc, yinc)
        case graph_menu_opt_help
    end select
      
next

call gt.cleanup_screen
!call gt.display_credit 

end program

!_______________________________________________________________________________
Sub gt.get_bounds(word xbound(), ybound(), xinc, yinc)
option constant type=Integer

declare string pinput, long ok
call smg$create_virtual_display(10,20, scrn::popup, smg$m_border)
call smg$paste_virtual_display(scrn::popup, scrn::pboard, 6, 35)

call smg$read_string(scrn::kboard, pinput, "X Upper Bounds: ",3,,,,,, &
                     scrn::popup,,,,) 
xbound(upperbd) = integer(pinput,word)
call smg$read_string(scrn::kboard, pinput, "X Lower Bounds: ",3,,,,,,&
                     scrn::popup,,,,) 
xbound(lowerbd) = integer(pinput,word)
call smg$read_string(scrn::kboard, pinput, "Y Upper Bounds: ",3,,,,,,&
                     scrn::popup,,,,) 
ybound(upperbd) = integer(pinput,word)
call smg$read_string(scrn::kboard, pinput, "Y Lower Bounds: ",3,,,,,,&
                     scrn::popup,,,,) 
ybound(lowerbd) = integer(pinput,word)
call smg$read_string(scrn::kboard, pinput, "X Increment: ",3,,,,,,&
                     scrn::popup,,,,)         
xinc = integer(pinput,word)
call smg$read_string(scrn::kboard, pinput, "Y Increment: ",3,,,,,,&
                     scrn::popup,,,,) 
yinc = integer(pinput,word)
call smg$unpaste_virtual_display(scrn::popup, scrn::pboard)

End Sub

!_______________________________________________________________________________
Sub gt.get_formula(formula_typ. curr_formula)
option constant type=Integer

declare word selection
call smg$paste_virtual_display(scrn::formula, scrn::pboard, 6, 35)
call smg$select_from_menu(scrn::kboard, scrn::formula, selection, &
                        !default_choice!, &
                        !flags!, &
                        !help lib!, &
                        !time_out in sec!, &
                        !terminator!, &
                        !selected_item!, &
                        !rend!smg$m_bold, &
                        !rend-com!) 

curr_formula = formula_typ.fr_v(integer(selection,long))
call smg$unpaste_virtual_display(scrn::formula, scrn::pboard)

End Sub

!_______________________________________________________________________________
Sub gt.execute_graph(formula_typ. curr_formula, word xbound(), ybound(), xinc, &
                     yinc)
option constant type=Integer

declare gfloat calcs(1 to 1000), posg, relx, rely, posx, posy, long incr, i

!Clear Screen
!------------
call smg$erase_display(scrn::main)

!Calculate formula
!-----------------
incr = 1
posg = xbound(lowerbd)

select formula_typ.n(curr_formula)
    case graph_formula_typ_sin
        calcs(incr) = sin(posg) * 10
        while posg <> xbound(upperbd)
            incr = incr + 1
            posg = posg + xinc
            calcs(incr) = sin(posg) * 10
        next
    case graph_formula_typ_cos
        calcs(incr) = cos(posg) * 10
        while posg <> xbound(upperbd)
            incr = incr + 1
            posg = posg + xinc
            calcs(incr) = cos(posg) * 10
        next
    case graph_formula_typ_tan
        calcs(incr) = tan(posg) * 10
        while posg <> xbound(upperbd)
            incr = incr + 1
            posg = posg + xinc
            calcs(incr) = tan(posg) * 10
        next
    case graph_formula_typ_log
        while posg <= 0
            incr = incr + 1
            posg = posg + xinc
        next
        calcs(incr) = log(posg)
        while posg <> xbound(upperbd)
            incr = incr + 1
            posg = posg + xinc
            calcs(incr) = log(posg)
        next
    case graph_formula_typ_paro
        calcs(incr) = posg*posg
        while posg <> xbound(upperbd)
            incr = incr + 1
            posg = posg + xinc
            calcs(incr) = posg*posg
        next
end select

!Draw X axis
!-----------
rely = (ybound(upperbd) + ybound(lowerbd)) / &
       (abs(ybound(upperbd)) + abs(ybound(lowerbd)))
rely = 0.5 if rely = 0
if rely > -1 and rely < 1 then
    rely = 1 - abs(rely) if rely < 0
    call smg$draw_line(scrn::main, (gw_height+1)*rely, 2, (gw_height+1)*rely, &
                       gw_length-4)
    call smg$draw_char(scrn::main, smg$m_down or smg$m_up or smg$m_right, &
                       (gw_height+1)*rely, 1)
    call smg$draw_char(scrn::main, smg$m_up or smg$m_left or smg$m_down, &
                       (gw_height+1)*rely, gw_length-3)
    call smg$put_chars(scrn::main, str$(xbound(lowerbd)), &
                       ((gw_height+1)*rely)+1,1)
    call smg$put_chars(scrn::main, str$(xbound(upperbd)), &
                       ((gw_height+1)*rely)+1, gw_length-5)
end if

!Draw Y axis
!-----------
relx = (xbound(upperbd) + xbound(lowerbd)) / &
       (abs(xbound(upperbd)) + abs(xbound(lowerbd)))
relx = 0.5 if relx = 0
if relx > -1 and relx < 1 then
    if relx > 0 then
        relx = 1 - relx
    else
        relx = abs(relx)
    end if
    call smg$draw_line(scrn::main, 2, (gw_length-1)*relx, gw_height-1, &
                       (gw_length-1)*relx)
    call smg$draw_char(scrn::main, smg$m_down or smg$m_left or smg$m_right, &
                       1, (gw_length-1)*relx)
    call smg$draw_char(scrn::main, smg$m_up or smg$m_left or smg$m_right, &
                       gw_height, (gw_length-1)*relx)
    call smg$put_chars(scrn::main, str$(ybound(upperbd)),1, &
                       ((gw_length-1)*relx)-3)
    call smg$put_chars(scrn::main, str$(ybound(lowerbd)),gw_height, &
                       ((gw_length-1)*relx)-3)
end if

!Plot Formula
!------------
for i = 1 to incr
    posy = calcs(i)
    posx = i / (abs(xbound(upperbd)) + abs(xbound(lowerbd)))
    if ybound(upperbd) > posy and ybound(lowerbd) < posy then
        posy = (posy + abs(ybound(upperbd)) + abs(ybound(lowerbd)) - &
               ybound(upperbd)) / (abs(ybound(upperbd)) + abs(ybound(lowerbd)))
        call smg$put_chars(scrn::main, point_char, gw_height* (1- posy), &
                           (gw_length-10)*posx)
    end if
next i
End Sub

!_______________________________________________________________________________
Sub gt.init_screen
option constant type=Integer

! Create and paste screens
!------------------------- 
declare Long rows, cols, byte i
map (menu_opts) string choices(1 to graph_menu_opt_exit)=10, &
                string formula(1 to graph_formula_typ_paro)=15

for i = lbound(choices) to ubound(choices)
    choices(i) = menu_opt.label(menu_opt.fr_v(i))
next i

for i = lbound(formula) to ubound(formula)
    formula(i) = formula_typ.formula(formula_typ.fr_v(i))
next i

call smg$create_pasteboard(scrn::pboard,, rows, cols)
call smg$create_virtual_keyboard(scrn::kboard, "sys$input:")
call smg$create_virtual_display(1, cols, scrn::title)
call smg$create_virtual_display(15,61, scrn::help, smg$m_block_border)
call smg$label_border(scrn::help, help_win,,,smg$m_bold or smg$m_user5)
call smg$create_virtual_display(gw_height,gw_length, scrn::main, smg$m_border)
call smg$label_border(scrn::main, main_win,,,smg$m_bold)
call smg$create_virtual_display(gw_height,menu_length, scrn::menu, smg$m_border)
call smg$create_virtual_display(10,20, scrn::formula, smg$m_border)
call smg$label_border(scrn::formula, formula_win,,,smg$m_bold)
call smg$paste_virtual_display(scrn::title, scrn::pboard, 1, 1)
call smg$paste_virtual_display(scrn::main, scrn::pboard, 3, 2)
call smg$paste_virtual_display(scrn::menu, scrn::pboard, 3, cols-menu_length)
call smg$create_menu(scrn::menu, choices(), smg$k_vertical, &
                     smg$m_double_space or smg$m_wrap_menu, 2, smg$m_bold)
call smg$create_menu(scrn::formula, formula(), smg$k_vertical, &
                     smg$m_wrap_menu, 2, smg$m_bold)

! Fill out screens with useful information
!-----------------------------------------
call smg$put_chars_wide(scrn::title, graph_title,1,cols/2 - len(graph_title),  &
                                        SMG$M_BOLD or SMG$M_REVERSE)
call smg$put_chars(scrn::help, "The aim of the Game is to get Little Mario out of the",2,5,,)
call smg$put_chars(scrn::help, "deadly labyrinth and save the lovely Maid Marion.  To",3,5,,)
call smg$put_chars(scrn::help, "move Mario without dying you will have to jump up and",4,5,,)
call smg$put_chars(scrn::help, "over obstacles.  Gold coins can be found in the maze.",5,5,,)
call smg$put_chars(scrn::help, "There are 20 to be found.  Mario can apply  his super",6,5,,)
call smg$put_chars(scrn::help, "pinggy shoes to jump up high places. Only three pings",7,5,,)
call smg$put_chars(scrn::help, "are availble.  You must collect the key to unlock the",8,5,,)
call smg$put_chars(scrn::help, "door to save Marion. The map features are as follows:",9,5,,)

End Sub

!_______________________________________________________________________________
Sub gt.cleanup_screen
option constant type=Integer

! removed the screens
! -------------------
call smg$unpaste_virtual_display(scrn::main, scrn::pboard)   
call smg$delete_virtual_display(scrn::main)
call smg$unpaste_virtual_display(scrn::menu, scrn::pboard)   
call smg$delete_virtual_display(scrn::menu)
call smg$unpaste_virtual_display(scrn::title, scrn::pboard)   
call smg$delete_virtual_display(scrn::title)
call smg$delete_virtual_display(scrn::help)
call smg$delete_pasteboard(scrn::pboard)
End Sub                                       

!_______________________________________________________________________________
Sub gt.display_credit
option constant type=Integer, handle=severe

! Display closing message when quitting
!--------------------------------------

external Long function ots$cvt_tz_l(String !hex_str!, Long !val!, &
    Long by value !val_size!)
declare long z,bina,i
declare string hex

data "16","00","09","09","09","09","09","42","72","6F","75","67","68","74","20","74"
data "6F","20","79","6F","75","20","62","79","0D","0A","09","09","09","09","09","53"
data "4C","41","52","54","49","42","41","52","54","46","41","53","54","20","49","4E"
data "43","2E","0D","0A","0D","0A","20","5F","5F","5F","5F","20","20","20","20","20"
data "20","20","20","20","20","20","20","20","5F","5F","5F","5F","20","20","20","20"
data "20","20","20","20","20","20","20","20","20","20","20","20","20","20","20","20"
data "20","20","20","20","20","20","20","20","20","20","20","20","7C","2B","2B","2B"
data "2B","2B","7C","0D","0A","00","7C","2B","2B","2B","2B","7C","20","20","20","20"
data "5F","5F","5F","20","20","20","20","7C","2B","2B","2B","2B","7C","20","20","20"
data "20","20","20","20","20","20","20","20","20","20","20","20","20","20","20","20"
data "20","20","20","20","5F","5F","5F","5F","20","20","20","20","7C","2B","2B","2B"
data "2B","2B","7C","0D","0A","00","7C","2B","2B","2B","2B","7C","20","20","20","7C"
data "2B","2B","20","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F"
data "5F","5F","5F","5F","5F","5F","5F","5F","5F","20","20","20","20","20","20","20"
data "20","20","20","7C","2B","2B","2B","2B","7C","20","20","20","7C","2B","2B","2B"
data "2B","2B","7C","0D","0A","00","7C","2B","2B","2B","2B","7C","20","20","20","7C"
data "2B","2B","2F","20","20","20","20","20","20","2F","28","20","29","5C","20","20"
data "20","20","20","20","20","20","20","20","20","5C","20","20","20","20","20","20"
data "20","20","20","7C","2B","2B","2B","2B","7C","20","20","20","7C","2B","2B","2B"
data "2B","2B","7C","20","20","20","5F","5F","0D","0A","7C","20","20","20","20","7C"
data "20","20","20","7C","2B","7C","20","20","20","20","20","20","7C","2D","6F","6F"
data "20","2D","7C","20","20","20","20","20","20","20","20","20","20","20","5C","5F"
data "5F","5F","5F","5F","5F","20","20","7C","2B","2B","2B","2B","7C","20","20","20"
data "7C","2B","2B","2B","2B","2B","7C","20","20","7C","2B","2B","7C","0D","0A","00"
data "2D","2D","2D","2D","2D","28","5F","5F","29","2D","2D","7C","20","20","20","20"
data "20","20","20","5C","5F","5F","5C","2F","20","20","20","20","20","20","20","20"
data "20","20","20","5F","28","5F","5F","29","5F","20","20","5C","20","2D","2D","2D"
data "2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D"
data "2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","0D"
data "0A","00","20","6F","20","20","20","28","20","6F","6F","20","20","2F","5F","5F"
data "5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F"
data "5F","5F","5F","5F","5F","7C","28","6F","6F","29","20","20","5C","20","7C","20"
data "20","20","20","20","20","20","20","20","5F","5F","0D","0A","20","7C","20","20"
data "5F","2F","5C","5F","7C","20","7C","20","20","4D","20","4F","20","4F","20","2D"
data "20","42","20","55","20","53","20","54","20","45","20","52","20","53","7C","5F"
data "5F","5C","2F","5C","20","2F","7C","20","7C","20","20","20","20","20","20","20"
data "20","2F","6F","6F","7C","20","2D","20","42","6C","65","61","75","72","67","68"
data "21","0D","0A","00","20","7C","2D","7C","20","20","5C","5C","5F","5F","5F","5F"
data "20","20","20","20","20","20","20","20","20","20","20","20","20","20","20","20"
data "20","20","20","20","20","20","20","20","20","2D","2D","2D","2D","2D","2D","20"
data "20","29","5F","20","20","20","20","2F","7C","20","20","2F","5C","0D","0A","00"
data "20","20","2D","7C","5F","20","20","5C","5F","7C","2D","5F","7C","5E","5E","5E"
data "5E","5E","5E","5E","5E","5E","5E","5E","5E","5E","5E","5E","5E","5E","5E","5E"
data "5E","5E","5E","5E","5E","5E","5E","20","30","20","20","20","20","20","5F","7C"
data "20","20","2A","20","20","5C","2F","20","20","2A","0D","0A","20","20","20","20"
data "20","5C","20","20","7C","20","20","5F","5F","5F","5F","5F","5F","5F","5F","5F"
data "5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F","5F"
data "5F","5F","5F","5F","5F","5F","5F","5F","5F","2F","0D","0A","20","20","20","20"
data "20","7C","20","20","57","7C","20","20","5C","20","5C","5F","2F","20","2F","2D"
data "2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D","2D"
data "20","5C","20","5C","5F","2F","20","2F","0D","0A","20","20","20","20","20","2F"
data "20","2F","5C","20","5C","20","20","5C","5F","5F","5F","2F","20","20","20","20"
data "20","20","20","20","20","20","20","20","20","20","20","20","20","20","20","20"
data "5C","5F","5F","5F","2F","0D","0A","00","20","20","20","20","2F","20","2F","20"
data "20","5C","20","5C","0D","0A","20","20","20","20","5E","5E","5E","20","20","20"
data "5E","5E","5E","20","20","20","20","20","20","20","20","20","20","20","20","20"
data "20","20","20","20","20","20","20","20","20","20","20","20","20","20","20","20"
data "20","20","20","20","20","20","20","20","20","57","68","6F","20","79","6F","75"
data "20","67","6F","6E","6E","61","20","63","61","6C","6C","2E","2E","2E","3F","00"

for i = 1 to 16*59 
    read hex
    z = ots$cvt_tz_l(hex, bina, 4)
    print string$(1,bina);
next i

End sub

end implementation
end image

#.LNK
$! Output from FIX_LINK, 15-MAR-2002 15:56:45.44
$ link sys$input:/option /exe=genbld:graph_tool.exe 'P1'
genbld:graph_tool.obj, -
genbld:graph_tool.1obj, -
decbld:sis_swnlib.olb/library
$exit
