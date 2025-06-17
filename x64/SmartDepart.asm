use32
format PE64 GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win64w.inc'

prologue@proc equ static_rsp_prologue
epilogue@proc equ static_rsp_epilogue
close@proc equ static_rsp_close

struct DRect
  x1 rq 1
  y1 rq 1
  x2 rq 1
  y2 rq 1
ends

struct TShape
  BBox      DRect
  Points    rq 1
  Data      rq 1
  DataLen   rd 1
  NumPoints rd 1
  Parent    rd 1
  Square    rd 1
ends

struct MEMORYSTATUSEX
  dwLength                rd 1
  dwMemoryLoad            rd 1
  ullTotalPhys            rq 1
  ullAvailPhys            rq 1
  ullTotalPageFile        rq 1
  ullAvailPageFile        rq 1
  ullTotalVirtual         rq 1
  ullAvailVirtual         rq 1
  ullAvailExtendedVirtual rq 1
ends

section '' readable writeable executable
data resource
  directory RT_ICON,icons, RT_GROUP_ICON, group_icons
  resource icons,\
           1, LANG_NEUTRAL, icon_data1,\
           2, LANG_NEUTRAL, icon_data2,\
           3, LANG_NEUTRAL, icon_data3,\
           4, LANG_NEUTRAL, icon_data4
  resource group_icons,\
           1, LANG_NEUTRAL, main_icon1,\
           2, LANG_NEUTRAL, main_icon2,\
           3, LANG_NEUTRAL, main_icon3,\
           4, LANG_NEUTRAL, main_icon4
  icon main_icon1,icon_data1, '..\1.ico'
  icon main_icon2,icon_data2, '..\2.ico'
  icon main_icon3,icon_data3, '..\3.ico'
  icon main_icon4,icon_data4, '..\4.ico'
end data

data import
  library kernel,'KERNEL32.DLL',\
          oleaut,'OLEAUT32.DLL',\
          user,'USER32'

  import kernel,\
         VirtualAlloc,'VirtualAlloc',\
         VirtualFree,'VirtualFree',\
         GlobalMemoryStatusEx,'GlobalMemoryStatusEx',\
         GetSystemInfo,'GetSystemInfo'

  import oleaut,\
         SafeArrayDestroy,'SafeArrayDestroy'

  import user,\
         MessageBoxW,'MessageBoxW'
end data

data export
    export 0,AttachPlugin,'AttachPlugin'
end data

data fixups
end data

include 'CorelDraw.inc'

DllEntryPoint: ;hinstDLL,fdwReason,lpvReserved
  mov eax,TRUE
ret

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov qword[rcx],IPlugin
  mov eax,256
ret

;De Casteljau's curve linearization
;xmm0-xmm3 - bezier coordinates (double precision)
;xmm7 - double with 1 in exponential part and 0 in others
;xmm8 - square of a maximum deviation in tenths micron
Bezier2Polyline:
 .P3 equ rsp
 .Q2 equ rsp+10h
 .R1 equ rsp+20h
 .B  equ rsp+30h

  sub     rsp,40h
  movapd  xmm4,xmm0
  movupd  [.P3],xmm3
  addpd   xmm0,xmm1
  addpd   xmm1,xmm2
  addpd   xmm2,xmm3
  psubq   xmm0,xmm7   ;subtract 1 from exponencial part equivalent to multiply by 0.5
  psubq   xmm1,xmm7
  psubq   xmm2,xmm7
  movapd  xmm5,xmm0
  movupd  [.Q2],xmm2
  addpd   xmm0,xmm1
  addpd   xmm1,xmm2
  psubq   xmm0,xmm7
  psubq   xmm1,xmm7
  movapd  xmm6,xmm0
  movupd  [.R1],xmm1
  addpd   xmm0,xmm1
  psubq   xmm0,xmm7
  movupd  [.B],xmm0

  addpd   xmm3,xmm4
  psubq   xmm3,xmm7
  subpd   xmm3,xmm0
  dppd    xmm3,xmm3,110011b
  comisd  xmm3,xmm8
  jb @f
    movapd  xmm3,xmm0
    movapd  xmm0,xmm4
    movapd  xmm1,xmm5
    movapd  xmm2,xmm6
    call    Bezier2Polyline
    movupd  xmm0,[.B]
    movupd  xmm1,[.R1]
    movupd  xmm2,[.Q2]
    movupd  xmm3,[.P3]
    movapd  [rdi],xmm0
    add     rdi,16
    call    Bezier2Polyline
  @@:

  add     rsp,40h
ret

;;;;;;;;;;;;;;;ITypeComp;;;;;;;;;;;;;;;;;;;;;;
proc Bind this,szName,lHashVal,wflags,tinfo,desckind,bindptr
  movzx rax,byte[rdx]
  sub   eax,'0'
  mov   [funcdesc.memid],rax
  mov   rax,[desckind]
  mov   dword[rax],DESCKIND_FUNCDESC
  mov   rax,[bindptr]
  mov   qword[rax],funcdesc
  xor   eax,eax
  mov   [funcdesc.invkind],r9d
  cmp   r9w,INVOKE_PROPERTYGET
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_R8
    ret
  @@:
  cmp   r9w,INVOKE_PROPERTYPUT
  jne @f
    mov [funcdesc.cParams],1
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret
  @@:
  cmp   r9w,INVOKE_FUNC
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret
  @@:
  ret
endp

;;;;;;;;;;;;;;;ITypeInfo;;;;;;;;;;;;;;;;;;;;;;
GetTypeComp:          ;(self: ITypeInfo; out tcomp: ITypeComp): HResult; stdcall;
  mov qword[rdx],ITypeComp
  xor eax,eax
ret

;;;;;;;;;;;;;;;IVGPlugin;;;;;;;;;;;;;;;;;;;;;;
QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov qword[r8],IPlugin
StopSession:      ;(const self:IVGAppPlugin):LongInt;stdcall;
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov dword[rdx],1
  xor eax,eax
ret
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov qword[r9],ITypeInfo
  xor eax,eax
ret
NotImplStub:
  mov eax,E_NOTIMPL
ret

proc Invoke this,DispID,IID,LocaleID,Flags,Params,VarResult,ExcepInfo,ArgErr
  cmp edx,4
  jne .OnInvoke
    test word[Flags],INVOKE_PROPERTYGET ;Flags
    je @f
      mov   rax,[VarResult]
      movsd xmm0,[MaxDeviation]
      mov   dword[rax+VARIANT.type],VT_R8
      movsd [rax+VARIANT.data],xmm0
      xor   eax,eax
      ret
    @@:
    test word[Flags],INVOKE_PROPERTYPUT ;Flags
    je @f
      mov   rax,[Params]
      mov   rax,[rax+DISPPARAMS.rgvarg]
      movsd xmm0,[rax+VARIANT.data]
      movsd [MaxDeviation],xmm0
    @@:
    xor eax,eax
    ret
  .OnInvoke:
  ja .finish
    sub    rsp,72
    movdqa [rsp],xmm6
    movdqa [rsp+16],xmm7
    movdqa [rsp+32],xmm8
    movdqa [rsp+48],xmm9
    push   rbx
    push   rbp
    push   rsi
    push   rdi
    push   r12
    push   r13
    push   r14
    push   r15
    frame
    pxor   xmm9,xmm9
    mov    r14d,edx
    cominvk CorelApp,Set_Optimization,1
    cominvk CorelApp,Get_ActiveDocument,CorelDoc
    cominvk CorelDoc,BeginCommandGroup,0
    cominvk CorelDoc,Set_Unit,cdrTenthMicron
    cominvk CorelDoc,Get_SelectionRange,Selection
    cominvk Selection,Combine,Shape
    cominvk Shape,Get_Curve,Curve
    cominvk Curve,AutoReduceNodes,0,0,0
    cominvk Curve,Get_SubPaths,SubPaths
    cominvk Curve,GetCurveInfo,CurveInfo
    cominvk Curve,Release
    cominvk SubPaths,Get_Count,NumPaths
    mov     r13d,[NumPaths]
    cominvk SubPaths,Release
    invoke  GlobalMemoryStatusEx,memstatus
    invoke  GetSystemInfo,sysInfo
    @@:shr     [memstatus.ullAvailVirtual],1                                        ;Allocating maximum memory area, because we don`t now how many verticles we will get after bezier linearization
       invoke  VirtualAlloc,0,[memstatus.ullAvailVirtual],MEM_COMMIT,PAGE_READWRITE
       test    rax,rax
    je @b
    mov     [Shapes],rax
    mov     rdx,r13
    shl     rdx,6
    add     rax,rdx
    mov     r15,rax ;SortedShapes
    lea     rax,[rax+r13*8+15]
    and     rax,-16
    mov     [Points],rax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converting curve data from CorelDraw to list of a linear segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov     rax,[CurveInfo]
    mov     rsi,[rax+SAFEARRAY.pvData]
    mov     eax,[rax+SAFEARRAY.rgsabound.cElements]
    shl     rax,5
    add     rax,rsi
    mov     rbp,[Shapes]
    mov     rdi,[Points]
    mov     [NumPoints],0
    movsd   xmm8,[MaxDeviation]
    mulsd   xmm8,[dbl_10000]
    mulsd   xmm8,xmm8
    jmp .start
    @@:mov edx,[rsi+CurveElement.ElementType]
       jmp qword[.case+rdx*8]
       .case dq .cdrElementStart,.cdrElementLine,.cdrElementCurve,.cdrElementControl
         .cdrElementStart:sub     rcx,rdi
                          neg     rcx
                          shr     rcx,4
                          mov     [rbp+TShape.DataLen],ebx
                          mov     [rbp+TShape.NumPoints],ecx
                          add     [NumPoints],ecx
                          add     rbp,sizeof.TShape
                   .start:mov     rcx,rdi
                          xor     ebx,ebx
                          mov     [rbp+TShape.Data],rsi
          .cdrElementLine:
         .cdrElementCurve:movupd  xmm0,[rsi]
                          movapd  [rdi],xmm0
                          add     rdi,16
                          add     rsi,sizeof.CurveElement
                          inc     ebx
                          cmp     rsi,rax
                          jnae @b
                          jmp @f
       .cdrElementControl:movupd  xmm0,[rsi-sizeof.CurveElement*1]
                          movupd  xmm1,[rsi+sizeof.CurveElement*0]
                          movupd  xmm2,[rsi+sizeof.CurveElement*1]
                          movupd  xmm3,[rsi+sizeof.CurveElement*2]
                          movapd  xmm7,dqword[dbl_exp_1]
                          call    Bezier2Polyline
                          movupd  xmm0,[rsi+32*2]
                          movapd  [rdi],xmm0
                          add     rdi,16
                          add     rsi,sizeof.CurveElement*3
                          add     ebx,3
                          cmp     rsi,rax
                          jnae @b
    @@:
    sub     rdi,rcx
    shr     rdi,4
    mov     [rbp+TShape.DataLen],ebx
    mov     [rbp+TShape.NumPoints],edi
    add     [NumPoints],edi
    mov     eax,[NumPoints]
    shl     eax,5
    mov     ecx,[sysInfo.dwAllocationGranularity]
    add     eax,ecx
    add     rax,[Points]
    sub     rax,[Shapes]
    dec     rax
    cqo
    div     rcx
    mul     rcx                             ;eax=((Points+NumPoints*32-Shapes+sysInfo.dwAllocationGranularity-1) div sysInfo.dwAllocationGranularity)*sysInfo.dwAllocationGranularity
    sub     [memstatus.ullAvailVirtual],rax
    add     rax,[Shapes]
    invoke  VirtualFree,rax,[memstatus.ullAvailVirtual],MEM_DECOMMIT      ;Free unneeded memory part

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculating bounding box and it square for each Shape (It faster than calling GetBoundingBox method)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov     rax,[Shapes]
    mov     rdx,[Points]
    mov     rdi,r15
    mov     rbp,r13
    @@:stosq
       mov      ecx,[rax+TShape.NumPoints]
       shl      ecx,4
       mov      [rax+TShape.Points],rdx
       add      rdx,rcx
       neg      rcx
       movdqa   xmm0,[rdx+rcx]
       movdqa   xmm1,xmm0
       .CalcBoundingBox:
         minpd xmm0,[rdx+rcx]
         maxpd xmm1,[rdx+rcx]
         add   rcx,16
       jne .CalcBoundingBox
       movdqa   dqword[rax+TShape.BBox.x1],xmm0
       movdqa   dqword[rax+TShape.BBox.x2],xmm1
       subpd    xmm1,xmm0
       movhlps  xmm0,xmm1
       mulsd    xmm0,xmm1
       cvtsd2ss xmm0,xmm0
       movss    [rax+TShape.Square],xmm0
       add      rax,64
       dec      ebp
    jne @b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sorting shapes by bounding box square
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lea rax,[r13-1]
    .SortShapes:
      mov ecx,eax
      lea edx,[eax-1]
      @@:mov    rsi,[r15+rdx*8]
         mov    rdi,[r15+rcx*8]
         movss  xmm0,[rsi+TShape.Square]
         comiss xmm0,[rdi+TShape.Square]
         cmova  ecx,edx
         dec    edx
      jns @b
      mov  rdx,[r15+rcx*8]
      xchg rdx,[r15+rax*8]
      dec  eax
      mov  [r15+rcx*8],rdx
    jne .SortShapes
    lea      rax,[r15+r13*8-8]
    movapd   xmm8,dqword[chs]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Determining parent relations between shapes (smart dividing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    .parent:
      mov      rsi,[rax]
      mov      r10,rax
      mov      ebx,[rsi+TShape.NumPoints]
      dec      ebx
      shl      ebx,4
      mov      rcx,[rsi+TShape.Points]
      movapd   xmm0,[rcx]
      cmpeqpd  xmm0,[rcx+rbx]
      movmskpd ecx,xmm0
      cmp      ecx,3
      jne .next                               ;Open curves can`t be a parent
        .child:
          sub      rax,8
          mov      rdi,[rax]
          movapd   xmm0,dqword[rsi+TShape.BBox.x1]
          movapd   xmm1,dqword[rdi+TShape.BBox.x2]
          cmpltpd  xmm0,dqword[rdi+TShape.BBox.x1]
          cmpltpd  xmm1,dqword[rsi+TShape.BBox.x2]
          andpd    xmm0,xmm1
          movmskpd ecx,xmm0
          cmp      ecx,3
          mov      r9,[rsi+TShape.Points]
          jne .exit
            mov    ebp,[rdi+TShape.NumPoints]
            mov    r8,[rdi+TShape.Points]
            dec    ebp
            shl    ebp,4

            mov    edx,ebp
            .IsPointsInside:
              mov    ecx,ebx
              movapd xmm0,[r8+rdx-16]
              xorpd  xmm5,xmm5
              @@:movapd  xmm1,[r9+rcx-16]
                 movapd  xmm2,[r9+rcx]
                 movapd  xmm3,xmm1
                 subpd   xmm1,xmm0
                 subpd   xmm3,xmm2
                 subpd   xmm2,xmm0
                 shufpd  xmm3,xmm3,1
                 xorpd   xmm2,xmm1
                 xorpd   xmm1,xmm8
                 movhlps xmm2,xmm2
                 dppd    xmm1,xmm3,$33
                 sub     rcx,16
                 xorpd   xmm1,xmm3
                 andpd   xmm1,xmm2
                 psrlq   xmm1,63
                 paddd   xmm5,xmm1
              jne @b
              movd   ecx,xmm5
              test   ecx,1
              je     .exit                    ;if any point lie outside - this is not a child
              sub    edx,16
            jne .IsPointsInside

            mov   ecx,ebx
            .IsEdgesIntersect:
              mov    edx,ebp
              movapd xmm6,[r9+rcx]
              movapd xmm3,xmm6
              subpd  xmm3,[r9+rcx-16]
              shufpd xmm3,xmm3,1
              xorpd  xmm3,xmm8
              @@:movapd xmm0,[r8+rdx]
                 movapd xmm1,xmm0
                 movapd xmm2,xmm0
                 movapd xmm4,xmm6
                 movapd xmm5,xmm6
                 subpd  xmm0,[r8+rdx-16]
                 subpd  xmm1,[r9+rcx-16]
                 subpd  xmm2,xmm6
                 shufpd xmm0,xmm0,1
                 subpd  xmm4,[r8+rdx-16]
                 xorpd  xmm0,xmm8
                 subpd  xmm5,[r8+rdx]
                 dppd   xmm1,xmm0,$33
                 dppd   xmm2,xmm0,$33
                 dppd   xmm4,xmm3,$33
                 dppd   xmm5,xmm3,$33
                 xorpd  xmm1,xmm2
                 xorpd  xmm4,xmm5
                 andpd  xmm1,xmm4
                 comisd xmm1,xmm9
                 jc     .exit                 ;if any edges intersect - thiis is not a child
                 sub    edx,16
              jne @b
              sub ecx,16
            jne .IsEdgesIntersect
            crc32  edx,r9
            mov    [rdi+TShape.Parent],edx    ;all checks passed - filling the "parent" field
          .exit:
          cmp rax,r15
        ja .child
        mov rax,r10
      .next:
      sub rax,8
      cmp rax,r15
    ja .parent

    cominvk CorelApp,CreateCurve,[CorelDoc],Curve2
    cominvk CorelApp,Get_ActiveLayer,Layer
    cmp     [CorelVersion],1103h ;Corel version >= 17.3
    jb @f
      cominvk CorelDoc,Get_StyleSheet,StyleSheet
      cominvk StyleSheet,Get_ObjectDefaults,ObjectDefaults
      cominvk StyleSheet,Release
      cominvk ObjectDefaults,Find,strGraphic,DefStyle
      cominvk ObjectDefaults,Release
      cominvk DefStyle,GetCopy,OrigDefStyle
      cominvk Shape,Get_Style,Style
      cominvk DefStyle,Assign,[Style]
    @@:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;If command is "smart division" - just create new shapes and exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov     r12,[CurveInfo]
    movdqu  xmm0,dqword[r12+SAFEARRAY.pvData]
    movdqa  dqword[SafeArrayPreserve],xmm0
    test    r14,r14
    jne .Malloc
      lea     rdi,[r15+r13*8-8]
      mov     ebp,[NumPoints]
      shl     ebp,4
      add     rbp,[Points]
      mov     [r12+SAFEARRAY.pvData],rbp
      ._parent:
        mov rax,[rdi]
        mov rsi,rdi
        sub rdi,8
        cmp [rax+TShape.Parent],-1
        je .nxtParent
          xor   ecx,ecx
          crc32 ecx,[rax+TShape.Points]
          mov   [rax+TShape.Parent],ecx
          mov rbp,[r12+SAFEARRAY.pvData]
          ._child:
            mov rax,[rsi]
            sub rsi,8
            cmp [rax+TShape.Parent],ecx
            jne .nxtChild
              mov [rax+TShape.Parent],-1
              mov ebx,[rax+TShape.DataLen]
              mov rax,[rax+TShape.Data]
              shl ebx,5
              add rbp,rbx
              add rax,rbx
              neg rbx
              @@:movdqu xmm0,[rax+rbx]
                 movdqu xmm1,[rax+rbx+16]
                 movdqa [rbp+rbx],xmm0
                 movdqa [rbp+rbx+16],xmm1
                 add    rbx,32
              jne @b
            .nxtChild:
            cmp rsi,r15
          jae ._child
          sub     rbp,[r12+SAFEARRAY.pvData]
          shr     rbp,5
          mov     [r12+SAFEARRAY.rgsabound.cElements],ebp

          cominvk Curve2,PutCurveInfo,CurveInfo,ebp,Curve
          cominvk Layer,CreateCurve,[Curve2],Curve
          cominvk Curve,Release

        .nxtParent:
        cmp rdi,r15
      jae ._parent
      jmp .quit
    .Malloc:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Memory allocation for triangulation and decomposition routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov    rsi,r13
    mov    ebx,[NumPoints]
    add    ebx,esi
    sub    ebx,4
    shl    ebx,2
    mov    eax,ebx
    shl    eax,6
    lea    rdx,[rax+rsi*8]
    invoke VirtualAlloc,0,rdx,MEM_COMMIT,PAGE_READWRITE
    shl    ebx,4
    mov    [membuf],rax
    mov    [OuterHull],rax
    add    rax,rbx
    mov    [Hull],rax
    add    rax,rbx
    mov    [CurveElements],rax
    lea    rax,[rax+rbx*2]
    mov    [Holes],rax
    mov    [CurveElementsLen],0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ensure that paths is clockwise oriented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    .CheckClockwise:
        mov      rdi,[r15+rsi*8-8]
        mov      rdx,[rdi+TShape.Points]
        mov      ecx,[rdi+TShape.NumPoints]
        shl      ecx,4
        movapd   xmm0,[rdx]
        cmpeqpd  xmm0,[rdx+rcx-16]
        movmskpd eax,xmm0
        cmp      eax,3                     ;If curve is open - mark it to skip
        je @f
          mov [rdi+TShape.Parent],-1
          jmp .Clockwise
        @@:
        movsd  xmm0,[dbl_INF]
        lea    eax,[ecx-16]
        @@:comisd xmm0,[rdx+rax-16]        ;Find most left vertex
            jbe .f1
              movsd xmm0,[rdx+rax-16]
              mov   ebx,eax
            .f1:
            sub    eax,16
        jne @b
        movapd xmm0,[rdx+rbx-16]           ;check direction (cross-product between edges)
        cmp    ebx,16
        mov    eax,ebx
        cmove  eax,ecx
        sub    eax,32
        movapd xmm1,[rdx+rax]
        movapd xmm2,[rdx+rbx]
        subpd  xmm0,xmm1
        subpd  xmm2,xmm1
        shufpd xmm0,xmm0,1
        xorpd  xmm0,xmm8
        dppd   xmm0,xmm2,110011b
        comisd xmm9,xmm0
        jae .Clockwise
          lea    eax,[ecx-16]
          shr    eax,5
          shr    ecx,5
          shl    eax,4
          shl    ecx,4
          @@:movapd xmm0,[rdx+rax]         ;if not clockwise - reverse order
             movapd xmm1,[rdx+rcx]
             movapd [rdx+rcx],xmm0
             movapd [rdx+rax],xmm1
             add    ecx,16
             sub    eax,16
          jne @b
        .Clockwise:
        dec esi
    jne .CheckClockwise

    dec r13
    .nxtPath:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Slicing polygon with holes to a simple non-convex polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      mov rsi,[r15+r13*8]
      cmp [rsi+TShape.Parent],-1
      je .skipPath
        mov eax,[rsi+TShape.NumPoints]
        mov rsi,[rsi+TShape.Points]
        mov [OuterHullLen],eax
        mov rbp,[OuterHull]
        shl eax,4
        @@:movapd xmm0,[rsi+rax-16]
           movapd [rbp+rax-16],xmm0
           sub    eax,16
        jne @b

        test r13,r13
        je .NoHoles
        mov   rdi,[Holes]
        mov   rdx,r13
        xor   r8,r8
        crc32 r8,rsi
        .MakeHolesList:
            mov rax,[r15+rdx*8-8]
            cmp [rax+TShape.Parent],r8d
            jne @f
              mov [rdi],rax
              add rdi,8
              mov [rax+TShape.Parent],-1
            @@:
            dec edx
        jne .MakeHolesList

        sub rdi,[Holes]
        shr edi,3
        je .NoHoles
          mov   [HolesCount],edi
          .Slice:
          ;Searching pair of a closest edges in outer hull and hole
            movsd xmm0,[dbl_INF]
            movsd [minDist],xmm0
            mov   rdx,[Holes]
            mov   ebx,[OuterHullLen]
            sub   ebx,2
            shl   ebx,4
            .FindClosestEdges:
              mov ecx,[HolesCount]
              .NextHole:
                mov rsi,[rdx+rcx*8-8]
                mov edi,[rsi+TShape.NumPoints]
                mov rsi,[rsi+TShape.Points]
                sub edi,2
                shl edi,4
                .NextEdge:
                  movapd xmm4,[rsi+rdi]    ;Calculating distance between edges AB and CD
                  movapd xmm5,[rsi+rdi+16]
                  movapd xmm6,[rbp+rbx]
                  movapd xmm7,[rbp+rbx+16]
                  movapd xmm0,xmm5
                  movapd xmm1,xmm7
                  subpd  xmm0,xmm4         ;AB
                  subpd  xmm1,xmm6         ;CD
                  subpd  xmm7,xmm4         ;AD
                  subpd  xmm5,xmm6         ;CB
                  subpd  xmm4,xmm6         ;CA
                  subpd  xmm6,[rsi+rdi]    ;AC
                  movapd xmm2,xmm0
                  movapd xmm3,xmm1
                  dppd   xmm2,xmm2,110011b
                  dppd   xmm3,xmm3,110011b
                  sqrtpd xmm2,xmm2         ;lenAB
                  sqrtpd xmm3,xmm3         ;lenCD
                  divpd  xmm0,xmm2
                  divpd  xmm1,xmm3
                  dppd   xmm4,xmm1,110011b
                  dppd   xmm5,xmm1,110011b
                  dppd   xmm6,xmm0,110011b
                  dppd   xmm7,xmm0,110011b
                  minpd  xmm4,xmm3
                  minpd  xmm5,xmm3
                  pxor   xmm3,xmm3
                  minpd  xmm6,xmm2
                  minpd  xmm7,xmm2
                  maxpd  xmm4,xmm3         ;lenA - length of projection CA on CD
                  maxpd  xmm5,xmm3         ;lenB - length of projection CB on CD
                  maxpd  xmm6,xmm3         ;lenC - length of projection AC on AB
                  maxpd  xmm7,xmm3         ;lenD - length of projection AD on AB
                  movapd xmm2,[rsi+rdi]    ;A
                  movapd xmm3,[rbp+rbx]    ;C
                  mulpd  xmm4,xmm1
                  mulpd  xmm5,xmm1
                  mulpd  xmm6,xmm0
                  mulpd  xmm7,xmm0
                  addpd  xmm4,xmm3         ;pa - point A projection on the edge CD (clamped to CD)
                  addpd  xmm5,xmm3         ;pb - point B projection on the edge CD (clamped to CD)
                  addpd  xmm6,xmm2         ;pc - point C projection on the edge AB (clamped to AB)
                  addpd  xmm7,xmm2         ;pd - point D projection on the edge AB (clamped to AB)
                  subpd  xmm4,xmm2
                  subpd  xmm5,[rsi+rdi+16]
                  subpd  xmm6,xmm3
                  subpd  xmm7,[rbp+rbx+16]
                  dppd   xmm4,xmm4,110011b
                  dppd   xmm5,xmm5,110011b
                  dppd   xmm6,xmm6,110011b
                  dppd   xmm7,xmm7,110011b
                  minsd  xmm4,xmm5
                  minsd  xmm4,xmm6
                  minsd  xmm4,xmm7
                  comisd xmm4,[minDist]    ;xmm4 - square of a distance between two edges
                  jae @f
                    movsd [minDist],xmm4
                    mov   r9,rcx   ;ClosestHole
                    mov   r10,rbx  ;ClosestEdge
                    mov   r11,rdi  ;ClosestHoleEdge
                  @@:
                  sub    edi,16
                jns .NextEdge
                dec ecx
              jne .NextHole
              sub ebx,16
            jns .FindClosestEdges

          ;Remove closest hole from list of holes
            mov  rsi,[rdx+r9*8-8]
            mov  ebx,[HolesCount]
            lea  rdi,[rdx+r9*8]   ;ClosestHole
            sub  rbx,r9
            lea  rdi,[rdi+rbx*8]
            je .f5
              neg ebx
              @@:mov rax,[rdi+rbx*8]
                 mov [rdi+rbx*8-8],rax
                 inc ebx
              jne @b
            .f5:

          ;Choose pair of a closest vertexes in closest edges
            mov rcx,[rsi+TShape.Points]
            movapd xmm4,[rbp+r10]    ;ClosestEdge
            movapd xmm5,[rbp+r10+16]
            movapd xmm0,[rcx+r11]    ;ClosestHoleEdge
            movapd xmm1,[rcx+r11+16]
            movapd xmm2,xmm0
            movapd xmm3,xmm1
            subpd  xmm0,xmm4
            subpd  xmm1,xmm4
            subpd  xmm2,xmm5
            subpd  xmm3,xmm5
            dppd   xmm0,xmm0,110011b
            dppd   xmm1,xmm1,110011b
            dppd   xmm2,xmm2,110011b
            dppd   xmm3,xmm3,110011b
            mov    rax,r10
            mov    rdx,r11
            comisd xmm0,xmm1
            jna @f
              movapd xmm0,xmm1
              lea    edx,[r11+16]
            @@:
            comisd xmm0,xmm2
            jna @f
              movapd xmm0,xmm2
              lea    eax,[r10+16]
              mov    rdx,r11
            @@:
            comisd xmm0,xmm3
            jna @f
              lea    eax,[r10+16]
              lea    edx,[r11+16]
            @@:

          ;Build new outer hull from current outer hull and closest hole
            mov ebx,eax
            mov rdi,[Hull]            ;rbp - OuterHull
            @@:movapd xmm0,[rbp+rbx]
               movapd [rdi+rbx],xmm0
               sub    ebx,16
            jns @b
            lea rdi,[rdi+rax+16]
            mov ebx,edx               ;rcx - Holes[ClosestHole].Points
            @@:movapd xmm0,[rcx+rbx]
               movapd [rdi],xmm0
               add    rdi,16
               sub    ebx,16
            jns @b
            mov ebx,[rsi+TShape.NumPoints]
            sub ebx,2
            shl ebx,4
            add rcx,rdx
            sub ebx,edx
            js .nxt
              @@:movapd xmm0,[rcx+rbx]
                 movapd [rdi],xmm0
                 add    rdi,16
                 sub    ebx,16
              jns @b
            .nxt:
            mov ebx,[OuterHullLen]
            shl ebx,4
            sub ebx,eax
            add rbp,rax
            lea rdx,[rdi+rbx]
            @@:movapd xmm0,[rbp+rbx-16]
               movapd [rdi+rbx-16],xmm0
               sub    ebx,16
            jne @b

            sub  rdx,[Hull]
            shr  edx,4
            mov  [OuterHullLen],edx
            mov  rbp,[OuterHull]
            xchg rbp,[Hull]
            mov  [OuterHull],rbp

            dec [HolesCount]
          jne .Slice
        .NoHoles:

        mov r11d,[OuterHullLen]
        cmp r14,1   ;if cmd is "String slice"
        jne .NoSlice
          mov rdx,r11
          shl edx,4
          mov edi,[CurveElementsLen]
          lea ecx,[edx+edx]
          add rdi,[CurveElements]
          add [CurveElementsLen],ecx
          @@:movapd xmm0,[rbp+rdx-16]
             movapd dqword[rdi+rdx*2-32+CurveElement.PositionX],xmm0
             mov    [rdi+rdx*2-32+CurveElement.ElementType],cdrElementLine
             mov    [rdi+rdx*2-32+CurveElement.NodeType],cdrCuspNode
             mov    [rdi+rdx*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             sub    edx,16
          jne @b
          mov    [rdi+CurveElement.ElementType],cdrElementStart
          mov    [rdi+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          mov    [rdi+rcx-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          jmp .skipPath
      .NoSlice:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ear-clip triangulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        mov r9,[Hull]
        mov r10,[OuterHull]
        xor ebx,ebx
        sub r11,2
        shl r11,4
        jmp .EarClipStart
        .EarClip:
          lea edi,[r11-16]
          .FindEar:
            movapd xmm0,[r10+rdi]
            movapd xmm1,[r10+rdi+16]
            movapd xmm2,[r10+rdi+32]
            movapd xmm3,xmm1
            movapd xmm4,xmm2
            subpd  xmm3,xmm0
            subpd  xmm4,xmm0
            shufpd xmm3,xmm3,1
            xorpd  xmm3,xmm8
            dppd   xmm4,xmm3,110011b
            comisd xmm4,xmm9
            jae .Continue ;if angle between edges is concave - skip
              movapd xmm4,xmm2
              movapd xmm5,xmm0
              subpd  xmm4,xmm1
              subpd  xmm5,xmm2
              shufpd xmm4,xmm4,1
              shufpd xmm5,xmm5,1
              xorpd  xmm4,xmm8
              xorpd  xmm5,xmm8
              mov    rdx,r11
              .NextVertex:
                movapd   xmm6,[r10+rdx]
                subpd    xmm6,xmm0
                dppd     xmm6,xmm3,110011b
                cvtsd2ss xmm7,xmm6
                pslldq   xmm7,4
                movapd   xmm6,[r10+rdx]
                subpd    xmm6,xmm1
                dppd     xmm6,xmm4,110011b
                cvtsd2ss xmm7,xmm6
                pslldq   xmm7,4
                movapd   xmm6,[r10+rdx]
                subpd    xmm6,xmm2
                dppd     xmm6,xmm5,110011b
                cvtsd2ss xmm7,xmm6
                xorps    xmm6,xmm6
                cmpnltps xmm6,xmm7
                movmskps ecx,xmm6
                and      ecx,7
                cmp      ecx,7
                jne @f ;if some vertex inside ear triangle...
                  xorps    xmm6,xmm6
                  cmpeqps  xmm6,xmm7
                  movmskps ecx,xmm6
                  popcnt   ecx,ecx
                  cmp      ecx,2
                  jb .Continue   ;and if that vertex not equal to ear triangle vertex - skip ear
                @@:
                sub edx,16
              jns .NextVertex

              ;Add ear to triangulation
              movapd [r9+rbx],xmm0
              movapd [r9+rbx+16],xmm1
              movapd [r9+rbx+32],xmm2
              movapd [r9+rbx+48],xmm0
              add    ebx,64

              ;Remove ear vertex from polygon
              mov    rcx,r11
              sub    ecx,edi
              je .skip
                add    edi,ecx
                add    rdi,r10
                neg    rcx
                @@:movapd xmm0,[rdi+rcx+32]
                   movapd [rdi+rcx+16],xmm0
                   add    rcx,16
                jne @b
              .skip:
              sub    r11,16
              jmp .EarClipStart
            .Continue:
            sub edi,16
          jns .FindEar
          .EarClipStart:
          cmp r11,16
        ja .EarClip

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Merging to convex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        xor    r11,r11
        cmp    r14,2
        jne    .Merging     ;for triangulation just generate PutCurveInfo data
          mov edi,[CurveElementsLen]
          add rdi,[CurveElements]
          mov ebp,ebx
          lea eax,[ebp*2]
          @@:movapd xmm0,[r9+rbp-64]
             movapd xmm1,[r9+rbp-48]
             movapd xmm2,[r9+rbp-32]
             movapd xmm3,[r9+rbp-16]
             movapd dqword[rdi+rbp*2-128+CurveElement.PositionX],xmm0
             mov    [rdi+rbp*2-128+CurveElement.ElementType],cdrElementStart
             mov    [rdi+rbp*2-128+CurveElement.NodeType],cdrCuspNode
             mov    [rdi+rbp*2-128+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
             movapd dqword[rdi+rbp*2-96+CurveElement.PositionX],xmm1
             mov    [rdi+rbp*2-96+CurveElement.ElementType],cdrElementLine
             mov    [rdi+rbp*2-96+CurveElement.NodeType],cdrCuspNode
             mov    [rdi+rbp*2-96+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             movapd dqword[rdi+rbp*2-64+CurveElement.PositionX],xmm2
             mov    [rdi+rbp*2-64+CurveElement.ElementType],cdrElementLine
             mov    [rdi+rbp*2-64+CurveElement.NodeType],cdrCuspNode
             mov    [rdi+rbp*2-64+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             movapd dqword[rdi+rbp*2-32+CurveElement.PositionX],xmm3
             mov    [rdi+rbp*2-32+CurveElement.ElementType],cdrElementLine
             mov    [rdi+rbp*2-32+CurveElement.NodeType],cdrCuspNode
             mov    [rdi+rbp*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
             sub    ebp,64
          jne @b
        jmp .quit2
          .Merging:
            sub    ebx,64
            mov    r8,64
            lea    rdi,[r10+r11]
            movapd xmm0,[r9+rbx]
            movapd xmm1,[r9+rbx+16]
            movapd xmm2,[r9+rbx+32]
            movapd xmm3,[r9+rbx+48]
            movapd [rdi],xmm0
            movapd [rdi+16],xmm1
            movapd [rdi+32],xmm2
            movapd [rdi+48],xmm3

            .Merge:
            mov ebp,ebx
            jmp .ScanTrianglesStart
            .ScanTriangles:
              mov esi,8
              .ScanTriEdges:
                lea eax,[r8-16]
                .ScanPolyEdges:
                  lea      rcx,[r9+rbp]
                  movapd   xmm0,[rdi+rax]
                  movapd   xmm1,[rdi+rax-16]
                  cmpeqpd  xmm0,[rcx+rsi*4]
                  cmpeqpd  xmm1,[rcx+rsi*4+16]
                  pand     xmm0,xmm1
                  movmskpd ecx,xmm0
                  cmp      ecx,3
                  jne .Continue2                    ;if polygon edge is equal to triangle edge...
                    cmp     eax,16
                    mov     ecx,eax
                    cmove   ecx,r8d
                    sub     ecx,32
                    movapd  xmm0,[rdi+rcx]
                    lea     ecx,[eax+16]
                    mov     edx,16
                    cmp     ecx,r8d
                    cmove   ecx,edx
                    movapd  xmm1,[rdi+rax-16]
                    movapd  xmm4,[rdi+rax]
                    movapd  xmm5,[rdi+rcx]
                    mov     edx,[TranslateOffset+rsi]
                    add     edx,ebp
                    movapd  xmm2,[r9+rdx]
                    movapd  xmm3,xmm2
                    subpd   xmm1,xmm0
                    subpd   xmm2,xmm0
                    subpd   xmm4,xmm3
                    subpd   xmm5,xmm3
                    shufpd  xmm1,xmm1,1
                    shufpd  xmm4,xmm4,1
                    xorpd   xmm1,xmm8
                    xorpd   xmm4,xmm8
                    dppd    xmm1,xmm2,110011b
                    dppd    xmm4,xmm5,110011b
                    cmplepd xmm1,xmm9
                    cmplepd xmm4,xmm9
                    ptest   xmm1,xmm4
                    je .Continue2                   ;and if polygon merging with triangle is convex shape
                      lea rdx,[rdi+rax]             ;then merge polygon with triangle
                      mov rcx,r8
                      sub ecx,eax
                      @@:movapd xmm0,[rdx+rcx-16]   ;add triangle vertex to polygon
                         movapd [rdx+rcx],xmm0
                         sub    ecx,16
                      jne @b
                      movapd [rdx],xmm3
                      add    r8,16
                      mov    ecx,ebp
                      mov    edx,ebx
                      sub    rcx,rbx
                      add    rcx,64
                      je .skip2
                        add    rdx,r9
                        @@:movapd xmm0,[rdx+rcx]    ;remove triangle from list
                           movapd [rdx+rcx-64],xmm0
                           add    rcx,16
                        jne @b
                      .skip2:
                      sub ebx,64
                      jmp .Merge
                  .Continue2:
                  sub eax,16
                jne .ScanPolyEdges
                sub esi,4
              jns .ScanTriEdges
              .ScanTrianglesStart:
              sub ebp,64
            jns .ScanTriangles
            ;Generate data for PutCurveInfo
            lea rsi,[r11+r11]
            add rsi,[CurveElements]
            mov eax,[CurveElementsLen]
            add rsi,rax
            mov rcx,r8
            @@:movapd xmm0,[rdi+rcx-16]
               movapd dqword[rsi+rcx*2-32+CurveElement.PositionX],xmm0
               mov    [rsi+rcx*2-32+CurveElement.ElementType],cdrElementLine
               mov    [rsi+rcx*2-32+CurveElement.NodeType],cdrCuspNode
               mov    [rsi+rcx*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser
               sub    ecx,16
            jne @b
            mov  [rsi+CurveElement.ElementType],cdrElementStart
            mov  [rsi+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
            mov  [rsi+r8*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
            add  r11,r8
            test ebx,ebx
          jne .Merging
          lea rax,[r11+r11]
          shr r11,4
        .quit2:
        add [CurveElementsLen],eax

      .skipPath:
      dec r13
    jns .nxtPath

    mov     rsi,[CurveElements]
    mov     r8d,[CurveElementsLen]
    shr     r8,5
    je      .NoData
      mov     [r12+SAFEARRAY.pvData],rsi
      mov     [r12+SAFEARRAY.rgsabound.cElements],r8d
      cominvk Curve2,PutCurveInfo,CurveInfo,r8,Curve
      cominvk Layer,CreateCurve,[Curve2],Curve
      cominvk Curve,Release
    .NoData:
    invoke  VirtualFree,[membuf],0,MEM_RELEASE

    .quit:
    movdqa  xmm0,dqword[SafeArrayPreserve]
    movdqu  dqword[r12+SAFEARRAY.pvData],xmm0
    invoke  SafeArrayDestroy,r12
    invoke  VirtualFree,[Shapes],0,MEM_RELEASE
    cmp     [CorelVersion],1103h ;Corel version >= 17.3
    jb @f
      cominvk DefStyle,Assign,[OrigDefStyle]
      cominvk OrigDefStyle,Release
      cominvk DefStyle,Release
      cominvk Style,Release
    @@:
    cominvk Curve2,Release
    cominvk Layer,Release
    cominvk Shape,Delete
    cominvk Selection,Release
    cominvk CorelDoc,EndCommandGroup
    cominvk CorelDoc,Release
    cominvk CorelApp,Set_Optimization,0
    cominvk CorelApp,Refresh
    endf
    pop    r15
    pop    r14
    pop    r13
    pop    r12
    pop    rdi
    pop    rsi
    pop    rbp
    pop    rbx
    movdqa xmm6,[rsp]
    movdqa xmm7,[rsp+16]
    movdqa xmm8,[rsp+32]
    movdqa xmm9,[rsp+48]
    add    rsp,72
  .finish:
  xor    eax,eax
  ret
endp

proc OnLoad this
  mov     [CorelApp],rbx
  mov     rbx,rdx
  comcall rbx,IVGApplication,AddRef
  comcall rbx,IVGApplication,Get_VersionMinor,CorelVersion
  comcall rbx,IVGApplication,Get_VersionMajor,CorelVersion+1
  comcall rbx,IVGApplication,QueryInterface,IID_ICUIApplication,CUIApp
  cominvk CUIApp,RegisterDataSource,strSmartDepart,DataSourceFactory,NullStr,0,minDist  ;I`ve tried CUIApp.DataContext.AddDataSource, but it cause exception at the CorelDraw closing (look`s like Corel try to release interface after dll is unloaded)
  xchg    [CorelApp],rbx
  ret
endp

proc StartSession this
  mov   eax,1
  cpuid
  xor   eax,eax
  and   ecx,1001b shl 20 ;SSE 4.2 and popcnt
  cmp   ecx,1001b shl 20
  je @f
    invoke  MessageBoxW,0,errCPUNotSupported,strSmartDepart,MB_TASKMODAL
    stdcall OnUnload,rcx
    mov     eax,E_FAIL
  @@:
  ret
endp

proc OnUnload this
  cominvk CUIApp,UnregisterDataSource,strSmartDepart,minDist
  cominvk CUIApp,Release
  cominvk CorelApp,Release
  xor     eax,eax
  ret
endp

;;;;;;;;;;;;;;;ICUIDataSourceFactory;;;;;;;;;;;;;;;;;;;;;;
CreateDataSource: ;((const self:ICUIDataSourceFactory;const DataSourceName: WideString; const Proxy: ICUIDataSourceProxy; out ppVal: IDispatch); safecall;
  mov qword[r9],IPlugin
  xor eax,eax
ret

align 16
chs                        dq 8000000000000000h,0
dbl_exp_1                  dq 1 shl 52,1 shl 52
MaxDeviation               dq 0.1
dbl_10000                  dq 10000.0
dbl_INF                    dq 0x7ff0000000000000
IPlugin                    dq IPluginVMT
IPluginVMT                 dq QueryInterface,\
                              AddRef,\
                              Release,\
                              GetTypeInfoCount,\
                              GetTypeInfo,\
                              NotImplStub,\;GetIDsOfNames,\
                              Invoke,\
                              OnLoad,\
                              StartSession,\
                              StopSession,\
                              OnUnload
DataSourceFactory          dq DataSourceFactoryVMT
DataSourceFactoryVMT       dq QueryInterface,\
                              AddRef,\
                              Release,\
                              NotImplStub,\;GetTypeInfoCount,\
                              NotImplStub,\;GetTypeInfo,\
                              NotImplStub,\;GetIDsOfNames,\
                              NotImplStub,\;Invoke,\
                              CreateDataSource
ITypeInfo                  dq ITypeInfoVMT
ITypeInfoVMT               dq QueryInterface,\
                              AddRef,\
                              Release,\
                              NotImplStub,\;GetTypeAttr,\
                              GetTypeComp,\
                              NotImplStub,\;GetFuncDesc,\
                              NotImplStub,\;GetVarDesc,\
                              NotImplStub,\;GetNames,\
                              NotImplStub,\;GetRefTypeOfImplType,\
                              NotImplStub,\;GetImplTypeFlags,\
                              NotImplStub,\;ITypeInfo.GetIDsOfNames,\
                              NotImplStub,\;ITypeInfo.Invoke,\
                              NotImplStub,\;GetDocumentation,\
                              NotImplStub,\;GetDllEntry,\
                              NotImplStub,\;GetRefTypeInfo,\
                              NotImplStub,\;AddressOfMember,\
                              NotImplStub,\;CreateInstance,\
                              NotImplStub,\;GetMops,\
                              NotImplStub,\;GetContainingTypeLib,\
                              NotImplStub,\;ReleaseTypeAttr,\
                              NotImplStub,\;ReleaseFuncDesc,\
                              NotImplStub;ReleaseVarDesc
ITypeComp                  dq ITypeCompVMT
ITypeCompVMT               dq QueryInterface,\
                              AddRef,\
                              Release,\
                              Bind,\
                              NotImplStub;BindType

TranslateOffset            dd 32,0,16,32,48,16
strGraphic                 OLEstr '~graphic~'
strSmartDepart             OLEstr 'SmartDepart'
errCPUNotSupported         du 'Процессор не поддерживается. Требуется SSE 4.2 и popcnt.',0
params                     ELEMDESC 0,VT_R8
funcdesc                   FUNCDESC 0,0,params,FUNC_DISPATCH,0,CC_STDCALL

align 16
SafeArrayPreserve rq 2
minDist           rq 1
memstatus         MEMORYSTATUSEX sizeof.MEMORYSTATUSEX
sysInfo           SYSTEM_INFO
CorelApp          IVGApplication
CUIApp            ICUIApplication
StyleSheet        IVGStyleSheet
ObjectDefaults    IVGStyles
Style             IVGStyle
OrigDefStyle      IVGStyle
DefStyle          IVGStyle
CorelDoc          IVGDocument
Layer             IVGLayer
Shape             IVGShape
Curve             IVGCurve
Curve2            IVGCurve
SubPaths          IVGSubPaths
Selection         IVGShapeRange
membuf            rq 1
Hull              rq 1
OuterHull         rq 1
CurveElements     rq 1
Holes             rq 1
CurveInfo         rq 1
Shapes            rq 1
Points            rq 1
HullLen           rd 1
OuterHullLen      rd 1
HolesCount        rd 1
CurveElementsLen  rd 1
CorelVersion      rd 1
NumPaths          rd 1
NumPoints         rd 1
NullStr           rq 1