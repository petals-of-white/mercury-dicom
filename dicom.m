/*
    dicom.m
    Parsing DICOM files using lists of bytes (list(uint8))
*/
:- module dicom.

:- interface.
:- import_module io, string, list, array, char.
% :- import_module string.
% :- import_module list.
% :- import_module array.

% DICOM tag
:- type tag ---> tag(group::uint16, element::uint16).

% Value length
:- type vl == int64.

% Value representation
:- type vr --->
    ae; 
    as;
    at;
    cs;
    da;
    ds;
    dt;
    fd;
    fl;
    is_;
    lo;
    lt;
    ob;
    of;
    ow;
    pn;
    sh;
    sl;
    sq;
    ss;
    st;
    tm;
    ui;
    ul;
    un;
    us;
    ut.

% DICOM Element
:- type dicom_element ---> dicom_element(
    element_tag::tag,   
    element_vl::vl,
    element_vr::vr,
    element_content::array(uint8)).

% Read a file as byte list
:- pred read_file_byte_list(string::in, io.result(list(uint8))::out, io::di, io::uo) is det.

% Read dicom from a file
% :- pred read_dicom_from_file(string::in, maybe() io::di, io::uo) is det.

% Parse a DICOM OBJECT
:- pred read_dicom_object(list(dicom_element), list(uint8), list(uint8)).
:- mode read_dicom_object(out, in, out) is semidet.

% Parse a single dicom element
:- pred read_dicom_element(dicom_element, list(uint8), list(uint8)).
:- mode read_dicom_element(out, in, out) is semidet.

% Parse a dicom tag
:- pred read_dicom_tag(tag, list(uint8), list(uint8)).
:- mode read_dicom_tag(out, in, out) is semidet.

% read dicom metadata
% :- pred read_dicom_meta(list(dicom_element), list(uint8), list(uint8)).
% :- mode read_dicom_meta(out, in, out) is semidet.

% Suceeds if header is valid, fails otherwise
:- pred read_dicom_header(list(uint8), list(uint8)).
:- mode read_dicom_header(in, out) is semidet.

% Parse VR
:- pred read_vr(vr, list(uint8), list(uint8)).
:- mode read_vr(out,in,out) is semidet.

% Parse value length
:- pred read_vl(vl::out, vr::in, list(uint8)::in, list(uint8)::out) is semidet.

% Undefined value length = 0xffffffff.
:- func undefined_value_length = vl.

% String representation of VR
:- pred string_to_vr(string::in, vr::out) is semidet.

% Reserved VLs
:- pred is_vl_reserved(vr::out) is multi.


% Generic parsing utilities
% #########################

% Skips N elements in list, fails if input's length is lesser then N
:- pred skip(int, list(T), list(T)).
:- mode skip(in, in, out) is semidet.

% Parse as many times as possible
:- pred read_all(pred(B, list(A), list(A)), list(B), list(A), list(A)).
:- mode read_all(pred(out, in, out) is semidet, out, in, out) is det.

% Almost like split_list
:- pred take(int, list(T), list(T), list(T)).
:- mode take(in, out, in, out) is semidet.

:- func uint8_to_char(uint8) = char.

% Parse uint16 little endian
:- pred uint16_le(uint16, list(uint8), list(uint8)).
:- mode uint16_le(out, in, out) is semidet.

% Parse uint32 little endian
:- pred uint32_le(uint32, list(uint8), list(uint8)).
:- mode uint32_le(out, in, out) is semidet.


:- implementation.

:- import_module uint16, uint8, uint32, uint64, integer, int64, int.
:- import_module stream.


is_vl_reserved(ob).
is_vl_reserved(ow).
is_vl_reserved(of).
is_vl_reserved(sq).
is_vl_reserved(ut).
is_vl_reserved(un).

string_to_vr(String, VR) :-
    Lowered = string.to_lower(String),
    Parsable = (Lowered = "is" -> "is_"; Lowered) ++ ".",
    Length = string.length(Parsable),
    StartPos = posn(1,0,0),
    io.read_from_string("some file", Parsable, Length, Res, StartPos, posn(1,0, Length)),
    Res = ok(VR).

undefined_value_length = 0xffffffffi64.

read_dicom_tag(Tag) --> uint16_le(Group), uint16_le(Element), {Tag = tag(Group,Element)}.

read_vr(VR) -->
    [First, Second], {
        list.map(uint8_to_char, [First, Second]) = VRCharList,
        VRString = string.from_char_list(VRCharList),
        string_to_vr(VRString,VR)}.

read_vl(VL, VR) -->
    if      {is_vl_reserved(VR)} 
    then    skip(2), uint32_le(VLu32), {VL = integer.det_to_int64(integer.from_uint32(VLu32))}
    else    uint16_le(VLu16), {VL = integer.det_to_int64(integer.from_uint16(VLu16))}.

read_dicom_element(DicomElement) -->
    read_dicom_tag(Tag), read_vr(VR), read_vl(VL, VR),
    (   if  {VL=undefined_value_length}
        then 
            {make_empty_array(Content)}
        else
            {N = int64.det_to_int(VL)},
            take(N, ContentList),
            {array.from_list(ContentList, Content)}
    ),
    {DicomElement = dicom_element(Tag, VL, VR, Content)}.

read_dicom_header -->
    skip(128), [D,I,C,M],
    {   HeaderCharList = list.map(uint8_to_char, [D,I,C,M]),
        HeaderCharList = ['D', 'I', 'C', 'M']
    }.

read_dicom_object(DicomObject) -->
    read_dicom_header, read_all(read_dicom_element, DicomObject).

read_file_byte_list(FileName, ListResult, !IO) :-
    open_binary_input(FileName, StreamRes, !IO),
    (
        StreamRes = ok(Stream),
        read_binary_file(Stream, IntListRes, !IO),
        (
            IntListRes = ok(IntList), ListResult = ok(list.map(uint8.det_from_int, IntList))
            ;
            IntListRes = eof, ListResult = eof
            ;
            IntListRes = error(Err), ListResult = error(Err)
        ), close_binary_input(Stream, !IO)
        ;
        StreamRes = error(Err), ListResult = error(Err)
    ).



skip(N) --> (if {(N < 1)} then [] else [_], skip(N-1)).

% read_all(P, [Head | Tail]) -->
%     P(Head), (read_all(P, Rest) -> {Tail = Rest}; {Tail = []}).

read_all(P, Result) --> read_all(P, [], Result).

take(N, Start, List, End) :- list.split_list(N, List, Start, End).

uint8_to_char(V) = char.det_from_int(uint8.to_int(V)).

uint16_le(Uint16) --> [Byte1, Byte2], {Uint16 = uint16.from_bytes_le(Byte1, Byte2)}.

uint32_le(Uint32) --> [Byte1, Byte2, Byte3, Byte4], {Uint32 = uint32.from_bytes_le(Byte1, Byte2, Byte3, Byte4)}.

:- pred read_all(pred(B, list(A), list(A)), list(B), list(B), list(A), list(A)).
:- mode read_all(pred(out, in, out) is semidet, in, out, in, out) is det.

read_all(P, Acc, Result) -->
    (P(Head) ->
        read_all(P, [Head | Acc], Result)
    ;
        {Result = reverse(Acc)}
    ).