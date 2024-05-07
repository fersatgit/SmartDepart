use32
format PE64 GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win64w.inc'

prologue@proc equ static_rsp_prologue
epilogue@proc equ static_rsp_epilogue
close@proc equ static_rsp_close

E_NOTIMPL            =80004001h
E_FAIL               =80004005h
SelectionChange      =11h
OnPluginCommand      =14h
OnUpdatePluginCommand=15h

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
  directory RT_ICON, icons, RT_GROUP_ICON, group_icons
  resource icons, 1, LANG_NEUTRAL, icon_data
  resource group_icons, 1, LANG_NEUTRAL, main_icon
  icon main_icon, icon_data, '..\1.ico'
end data

data import
  library kernel,'KERNEL32.DLL',\
          oleaut,'OLEAUT32.DLL',\
          user,'USER32'

  import kernel,\
         ExitProcess,'ExitProcess',\
         VirtualAlloc,'VirtualAlloc',\
         VirtualFree,'VirtualFree',\
         GlobalMemoryStatusEx,'GlobalMemoryStatusEx',\
         GetSystemInfo,'GetSystemInfo',\
         lstrcmpW,'lstrcmpW'

  import oleaut,\
         SysFreeString,'SysFreeString',\
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
  mulpd   xmm0,xmm7
  mulpd   xmm1,xmm7
  mulpd   xmm2,xmm7
  movapd  xmm5,xmm0
  movupd  [.Q2],xmm2
  addpd   xmm0,xmm1
  addpd   xmm1,xmm2
  mulpd   xmm0,xmm7
  mulpd   xmm1,xmm7
  movapd  xmm6,xmm0
  movupd  [.R1],xmm1
  addpd   xmm0,xmm1
  mulpd   xmm0,xmm7
  movupd  [.B],xmm0

  addpd   xmm3,xmm4
  mulpd   xmm3,xmm7
  subpd   xmm3,xmm0
  dppd    xmm3,xmm3,110011b
  comisd  xmm3,[precision]
  ja @f
    movapd   [rdi],xmm0
    add      rdi,16
    add      rsp,40h
    ret
  @@:

  movapd  xmm3,xmm0
  movapd  xmm0,xmm4
  movapd  xmm1,xmm5
  movapd  xmm2,xmm6
  call    Bezier2Polyline
  movupd  xmm0,[.B]
  movupd  xmm1,[.R1]
  movupd  xmm2,[.Q2]
  movupd  xmm3,[.P3]
  call    Bezier2Polyline

  add     rsp,40h
ret

QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov qword[r8],IPlugin
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
GetIDsOfNames:    ; this,IID,Names,NameCount,LocaleID,DispIDs
  mov eax,E_NOTIMPL
ret

proc Invoke uses rbx rsi rdi rbp r12 r13, this,DispID,IID,LocaleID,Flags,Params,VarResult,ExcepInfo,ArgErr
  cmp edx,SelectionChange
  je .SelectionChange
  cmp edx,OnPluginCommand
  je .OnPluginCommand
  cmp edx,OnUpdatePluginCommand
  je .OnUpdatePluginCommand
  xor eax,eax
  ret
        .SelectionChange:cominvk SelectionInfo,GetProperty,strNumSelected,varTmp
                         cmp     dword[varTmp.data],1
                         mov     [varTmp.data+8],0                                         ;variant type = VT_EMPTY
                         ja @f
                           cominvk WDocCommands,GetProperty,strCanBreakApart,varTmp.data+8 ;Store result in "Enabled" variable
                           xor     eax,eax
                           ret
                         @@:
                         cominvk SelectionInfo,GetProperty,strIsValidCombine,varTmp.data+8 ;Store result in "Enabled" variable
                         xor     eax,eax
                         ret
        .OnPluginCommand:mov    rbx,[Params]
                         mov    rbx,[rbx+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,[rbx+VARIANT.data],strSmartDepart
                         test   eax,eax
                         jne    .finish
                           cominvk CorelApp,Get_ActiveDocument,CorelDoc
                           cominvk CorelDoc,BeginCommandGroup,0
                           cominvk CorelDoc,Set_Unit,cdrTenthMicron
                           cominvk CorelDoc,Get_SelectionRange,Selection
                           cominvk Selection,Combine,Shape
                           cominvk Shape,Get_Curve,Curve
                           cominvk Curve,Get_SubPaths,SubPaths
                           cominvk Curve,GetCurveInfo,CurveInfo
                           cominvk Curve,Release
                           cominvk SubPaths,Get_Count,NumPaths
                           cominvk SubPaths,Release
                           invoke  GlobalMemoryStatusEx,memstatus
                           invoke  GetSystemInfo,sysInfo
                           @@:shr     [memstatus.ullAvailVirtual],1
                              invoke  VirtualAlloc,0,[memstatus.ullAvailVirtual],MEM_COMMIT,PAGE_READWRITE
                              test    rax,rax
                           je @b
                           mov     [Shapes],rax
                           mov     edx,[NumPaths]
                           shl     rdx,6
                           add     rax,rdx
                           mov     [SortedShapes],rax
                           mov     edx,[NumPaths]
                           lea     rax,[rax+rdx*8+15]
                           and     rax,-16
                           mov     [Points],rax

                           mov     rax,[CurveInfo]
                           mov     rsi,[rax+SAFEARRAY.pvData]
                           mov     eax,[rax+SAFEARRAY.rgsabound.cElements]
                           shl     rax,5
                           add     rax,rsi
                           mov     rbp,[Shapes]
                           mov     rdi,[Points]
                           mov     [NumPoints],0
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
                                                 movapd  xmm7,dqword[dbl_05]
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
                           invoke  VirtualFree,rax,[memstatus.ullAvailVirtual],MEM_DECOMMIT

                           mov     rax,[Shapes]
                           mov     rdx,[Points]
                           mov     rbx,[SortedShapes]
                           mov     rdi,rbx
                           mov     ebp,[NumPaths]
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

                           mov eax,[NumPaths]
                           dec eax
                           .SortShapes:
                             mov ecx,eax
                             lea edx,[eax-1]
                             @@:mov    rsi,[rbx+rdx*8]
                                mov    rdi,[rbx+rcx*8]
                                movss  xmm0,[rsi+TShape.Square]
                                comiss xmm0,[rdi+TShape.Square]
                                cmova  ecx,edx
                                dec    edx
                             jns @b
                             mov  rdx,[rbx+rcx*8]
                             xchg rdx,[rbx+rax*8]
                             dec  eax
                             mov  [rbx+rcx*8],rdx
                           jne .SortShapes

                           mov      rbx,[SortedShapes]
                           mov      eax,[NumPaths]
                           lea      rax,[rbx+rax*8-8]
                           movapd   xmm7,dqword[chs]

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
                             jne .next ;Если кривая не замкнута - она не может содержать дочерние кривые
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
                                        xorpd   xmm1,xmm7
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
                                     je     .exit
                                     sub    edx,16
                                   jne .IsPointsInside

                                   mov   ecx,ebx
                                   .IsEdgesIntersect:
                                     mov    edx,ebp
                                     movapd xmm6,[r9+rcx]
                                     movapd xmm3,xmm6
                                     subpd  xmm3,[r9+rcx-16]
                                     shufpd xmm3,xmm3,1
                                     xorpd  xmm3,xmm7
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
                                        xorpd  xmm0,xmm7
                                        subpd  xmm5,[r8+rdx]
                                        dppd   xmm1,xmm0,$33
                                        dppd   xmm2,xmm0,$33
                                        dppd   xmm4,xmm3,$33
                                        dppd   xmm5,xmm3,$33
                                        xorpd  xmm1,xmm2
                                        xorpd  xmm4,xmm5
                                        andpd  xmm1,xmm4
                                        comisd xmm1,qword[chs+8]
                                        jc     .exit
                                        sub    edx,16
                                     jne @b
                                     sub ecx,16
                                   jne .IsEdgesIntersect
                                   crc32  edx,r9
                                   mov    [rdi+TShape.Parent],edx
                                 .exit:
                                 cmp rax,[SortedShapes]
                               ja .child
                               mov rax,r10
                             .next:
                             sub rax,8
                             cmp rax,[SortedShapes]
                           ja .parent

                           cominvk CorelApp,CreateCurve,[CorelDoc],Curve2
                           cominvk Curve2,GetCurveInfo,CurveInfo2
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

                           mov     r12,[CurveInfo2]
                           mov     eax,[NumPaths]
                           mov     rdi,[SortedShapes]
                           lea     rdi,[rdi+rax*8-8]
                           mov     ebp,[NumPoints]
                           shl     ebp,4
                           add     rbp,[Points]
                           mov     r13,[r12+SAFEARRAY.pvData]
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
                                 cmp rsi,[SortedShapes]
                               jae ._child
                               sub     rbp,[r12+SAFEARRAY.pvData]
                               shr     rbp,5
                               mov     [r12+SAFEARRAY.rgsabound.cElements],ebp

                               cominvk Curve2,PutCurveInfo,CurveInfo2,ebp,Curve
                               cominvk Layer,CreateCurve,[Curve2],Curve
                               cominvk Curve,Release

                             .nxtParent:
                             cmp rdi,[SortedShapes]
                           jae ._parent

                           mov     [r12+SAFEARRAY.pvData],r13
                           mov     [r12+SAFEARRAY.rgsabound.cElements],0
                           invoke  SafeArrayDestroy,r12
                           invoke  SafeArrayDestroy,[CurveInfo]
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
                           cominvk CorelApp,Refresh

                         .finish:
                         xor eax,eax
                         ret
  .OnUpdatePluginCommand:mov    rbx,[Params]
                         mov    rbx,[rbx+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,[rbx+sizeof.VARIANT*2+VARIANT.data],strSmartDepart
                         test   eax,eax
                         jne    @f
                           mov rax,[rbx+sizeof.VARIANT*1+VARIANT.data]
                           mov edx,[Enabled]
                           mov [rax],dx
                         @@:
                         xor    eax,eax
                         ret
endp

proc OnLoad uses rbx ;this,Application
  mov     [CorelApp],rdx
  mov     rbx,rdx
  comcall rdx,IVGApplication,AddRef
  comcall rbx,IVGApplication,Get_VersionMinor,CorelVersion
  comcall rbx,IVGApplication,Get_VersionMajor,CorelVersion+1
  ret
endp

proc StartSession uses rbx     ;(const self:IVGAppPlugin):LongInt;stdcall;
  mov     eax,1
  cpuid
  test    ecx,1 shl 20 ;SSE 4.2
  je @f
    mov     rbx,[CorelApp]
    comcall rbx,IVGApplication,AddPluginCommand,strSmartDepart,strButtonCaption,strButtonCaption,varTmp.data
    comcall rbx,IVGApplication,AdviseEvents,IPlugin,EventsCookie
    comcall rbx,IVGApplication,QueryInterface,IID_ICUIApplication,CUIApp
    cominvk CUIApp,Get_DataContext,DataContext
    cominvk DataContext,GetDataSource,strSelectionInfoDatasource,SelectionInfo
    cominvk DataContext,GetDataSource,strWDocCommands,WDocCommands
    cominvk DataContext,Release
    cominvk CUIApp,Release
    xor     eax,eax
    ret
  @@:
  invoke MessageBoxW,0,errCPUNotSupported,strSmartDepart,MB_TASKMODAL
  mov    eax,E_FAIL
  ret
endp

proc StopSession      ;(const self:IVGAppPlugin):LongInt;stdcall;
  cominvk CorelApp,UnadviseEvents,[EventsCookie]
  cominvk SelectionInfo,Release
  cominvk WDocCommands,Release
  xor     eax,eax
  ret
endp

proc OnUnload         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CorelApp,Release
  xor     eax,eax
  ret
endp

align 16
IID_ICUIApplication        db 00ah,000h,0eeh,09ch,0a0h,042h,080h,059h,043h,0a3h,07ah,0a7h,014h,061h,048h,02ch
chs                        dq 8000000000000000h,0
dbl_05                     dq 0.5,0.5
precision                  dq 0.04
IPlugin                    dq IPluginVMT
IPluginVMT                 dq QueryInterface,\
                              AddRef,\
                              Release,\
                              GetTypeInfoCount,\
                              GetTypeInfo,\
                              GetIDsOfNames,\
                              Invoke,\
                              OnLoad,\
                              StartSession,\
                              StopSession,\
                              OnUnload
                           dd 18
strGraphic                 du '~graphic~',0
                           dd 22
strSmartDepart             du 'SmartDepart',0
                           dd 26
strButtonCaption           du 'Умное деление',0
                           dd 46
strSelectionInfoDatasource du 'SelectionInfoDatasource',0
                           dd 28
strWDocCommands            du 'WDocCommandsDS',0
                           dd 28
strIsValidCombine          du 'IsValidCombine',0
                           du 26
strCanBreakApart           du 'CanBreakApart',0
                           dd 22
strNumSelected             du 'NumSelected',0
errCPUNotSupported         du 'Процессор не поддерживается. Требуется SSE 4.2.',0

align 16
memstatus        MEMORYSTATUSEX sizeof.MEMORYSTATUSEX
sysInfo          SYSTEM_INFO
CorelApp         IVGApplication
CUIApp           ICUIApplication
DataContext      ICUIDataContext
SelectionInfo    ICUIDataSourceProxy
WDocCommands     ICUIDataSourceProxy
StyleSheet       IVGStyleSheet
ObjectDefaults   IVGStyles
Style            IVGStyle
OrigDefStyle     IVGStyle
DefStyle         IVGStyle
CorelDoc         IVGDocument
Layer            IVGLayer
Shape            IVGShape
Curve            IVGCurve
Curve2           IVGCurve
SubPaths         IVGSubPaths
Selection        IVGShapeRange
CurveInfo        rq 1
CurveInfo2       rq 1
Shapes           rq 1
SortedShapes     rq 1
Points           rq 1
EventsCookie     rq 1
CorelVersion     rd 1
NumPaths         rd 1
NumPoints        rd 1
varTmp           VARIANT
Enabled          rd 1
                 rd 3