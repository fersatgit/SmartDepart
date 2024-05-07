use32
format PE GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win32w.inc'

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
  Points    rd 1
  Data      rd 1
  DataLen   rd 1
  NumPoints rd 1
  Parent    rd 1
  Square    rd 1
  align     rq 1
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
ret 12

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov eax,[esp+4]
  mov dword[eax],IPlugin
  mov eax,256
ret 4

Bezier2Polyline:
 .P3 equ esp
 .Q2 equ esp+10h
 .R1 equ esp+20h
 .B  equ esp+30h

  sub     esp,40h
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
    movapd   [edi],xmm0
    add      edi,16
    add      esp,40h
    retn
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

  add     esp,40h
retn

QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov eax,[esp+12]
  mov dword[eax],IPlugin
  xor eax,eax
ret 12
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret 4
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 8
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 12
GetIDsOfNames:    ;(const self:IVGAppPlugin; const IID: TGUID; Names: Pointer;NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 24


Invoke:           ;(const self:IVGAppPlugin; DispID: Integer; const IID: TGUID; LocaleID: Integer;Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  mov eax,[esp+8]
  cmp eax,SelectionChange
  je .SelectionChange
  cmp eax,OnPluginCommand
  je .OnPluginCommand
  cmp eax,OnUpdatePluginCommand
  je .OnUpdatePluginCommand
  xor eax,eax
  ret 36
        .SelectionChange:cominvk SelectionInfo,GetProperty,strNumSelected,varTmp
                         cmp     dword[varTmp.data],1
                         ja @f
                           cominvk WDocCommands,GetProperty,strCanBreakApart,varTmp.data
                           xor     eax,eax
                           ret 36
                         @@:
                         cominvk SelectionInfo,GetProperty,strIsValidCombine,varTmp.data
                         xor     eax,eax
                         ret 36
        .OnPluginCommand:push   ebx
                         mov    ebx,[esp+28]
                         mov    ebx,[ebx+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,dword[ebx+VARIANT.data],strSmartDepart
                         test   eax,eax
                         jne    .finish
                           pushad
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
                           xor     eax,eax
                           cmp     eax,dword[memstatus.ullAvailVirtual+4]
                           sbb     eax,eax
                           or      dword[memstatus.ullAvailVirtual],eax
                           @@:shr     dword[memstatus.ullAvailVirtual],1
                              invoke  VirtualAlloc,0,dword[memstatus.ullAvailVirtual],MEM_COMMIT,PAGE_READWRITE
                              test    eax,eax
                           je @b
                           mov     [Shapes],eax
                           mov     edx,[NumPaths]
                           shl     edx,6
                           add     eax,edx
                           mov     [SortedShapes],eax
                           mov     edx,[NumPaths]
                           lea     eax,[eax+edx*4+15]
                           and     eax,-16
                           mov     [Points],eax

                           mov     eax,[CurveInfo]
                           mov     esi,[eax+SAFEARRAY.pvData]
                           mov     eax,[eax+SAFEARRAY.rgsabound.cElements]
                           shl     eax,5
                           add     eax,esi
                           mov     ebp,[Shapes]
                           mov     edi,[Points]
                           mov     [NumPoints],0
                           jmp .start
                           @@:mov edx,[esi+CurveElement.ElementType]
                              jmp dword[.case+edx*4]
                              .case dd .cdrElementStart,.cdrElementLine,.cdrElementCurve,.cdrElementControl
                                .cdrElementStart:sub ecx,edi
                                                 neg ecx
                                                 shr ecx,4
                                                 mov [ebp+TShape.DataLen],ebx
                                                 mov [ebp+TShape.NumPoints],ecx
                                                 add [NumPoints],ecx
                                                 add ebp,sizeof.TShape
                                          .start:mov ecx,edi
                                                 xor ebx,ebx
                                                 mov [ebp+TShape.Data],esi
                                 .cdrElementLine:
                                .cdrElementCurve:movupd   xmm0,[esi]
                                                 movapd   [edi],xmm0
                                                 add      edi,16
                                                 add      esi,sizeof.CurveElement
                                                 inc      ebx
                                                 cmp      esi,eax
                                                 jnae @b
                                                 jmp @f
                              .cdrElementControl:movupd   xmm0,[esi-sizeof.CurveElement*1]
                                                 movupd   xmm1,[esi+sizeof.CurveElement*0]
                                                 movupd   xmm2,[esi+sizeof.CurveElement*1]
                                                 movupd   xmm3,[esi+sizeof.CurveElement*2]
                                                 movapd   xmm7,dqword[dbl_05]
                                                 call     Bezier2Polyline
                                                 movupd   xmm0,[esi+32*2]
                                                 movapd   [edi],xmm0
                                                 add      edi,16
                                                 add      esi,sizeof.CurveElement*3
                                                 add      ebx,3
                                                 cmp      esi,eax
                                                 jnae @b
                           @@:
                           sub     edi,ecx
                           shr     edi,4
                           mov     [ebp+TShape.DataLen],ebx
                           mov     [ebp+TShape.NumPoints],edi
                           add     [NumPoints],edi
                           mov     eax,[NumPoints]
                           shl     eax,5
                           add     eax,[sysInfo.dwAllocationGranularity]
                           add     eax,[Points]
                           sub     eax,[Shapes]
                           dec     eax             ;eax=Points+NumPoints*32-Shapes+sysInfo.dwAllocationGranularity-1
                           cdq
                           div     [sysInfo.dwAllocationGranularity]
                           mul     [sysInfo.dwAllocationGranularity]
                           sub     dword[memstatus.ullAvailVirtual],eax
                           add     eax,[Shapes]
                           invoke  VirtualFree,eax,dword[memstatus.ullAvailVirtual],MEM_DECOMMIT

                           mov     eax,[Shapes]
                           mov     edx,[Points]
                           mov     ebx,[SortedShapes]
                           mov     edi,ebx
                           mov     ebp,[NumPaths]
                           @@:stosd
                              mov      ecx,[eax+TShape.NumPoints]
                              shl      ecx,4
                              mov      [eax+TShape.Points],edx
                              add      edx,ecx
                              neg      ecx
                              movdqa   xmm0,[edx+ecx]
                              movdqa   xmm1,xmm0
                              .CalcBoundingBox:
                                minpd xmm0,[edx+ecx]
                                maxpd xmm1,[edx+ecx]
                                add   ecx,16
                              jne .CalcBoundingBox
                              movdqa   dqword[eax+TShape.BBox.x1],xmm0
                              movdqa   dqword[eax+TShape.BBox.x2],xmm1
                              subpd    xmm1,xmm0
                              movhlps  xmm0,xmm1
                              mulsd    xmm0,xmm1
                              cvtsd2ss xmm0,xmm0
                              movss    [eax+TShape.Square],xmm0
                              add      eax,64
                              dec      ebp
                           jne @b

                           mov eax,[NumPaths]
                           dec eax
                           .SortShapes:
                             mov ecx,eax
                             lea edx,[eax-1]
                             @@:mov    esi,[ebx+edx*4]
                                mov    edi,[ebx+ecx*4]
                                movss  xmm0,[esi+TShape.Square]
                                comiss xmm0,[edi+TShape.Square]
                                cmova  ecx,edx
                                dec    edx
                             jns @b
                             mov  edx,[ebx+ecx*4]
                             xchg edx,[ebx+eax*4]
                             dec  eax
                             mov  [ebx+ecx*4],edx
                           jne .SortShapes

                           mov      ebx,[SortedShapes]
                           mov      eax,[NumPaths]
                           lea      eax,[ebx+eax*4-4]
                           movapd   xmm7,dqword[chs]

                           .parent:
                             mov      esi,[eax]
                             mov      [esp-12],eax
                             mov      ebx,[esi+TShape.NumPoints]
                             dec      ebx
                             shl      ebx,4
                             mov      ecx,[esi+TShape.Points]
                             movapd   xmm0,[ecx]
                             cmpeqpd  xmm0,[ecx+ebx]
                             movmskpd ecx,xmm0
                             cmp      ecx,3
                             jne .next ;Если кривая не замкнута - она не может содержать дочерние кривые
                               .child:
                                 sub      eax,4
                                 mov      edi,[eax]
                                 movapd   xmm0,dqword[esi+TShape.BBox.x1]
                                 movapd   xmm1,dqword[edi+TShape.BBox.x2]
                                 cmpltpd  xmm0,dqword[edi+TShape.BBox.x1]
                                 cmpltpd  xmm1,dqword[esi+TShape.BBox.x2]
                                 andpd    xmm0,xmm1
                                 movmskpd ecx,xmm0
                                 mov      [esp-8],esi
                                 cmp      ecx,3
                                 mov      esi,[esi+TShape.Points]
                                 jne .exit
                                   mov    [esp-4],edi
                                   mov    ebp,[edi+TShape.NumPoints]
                                   mov    edi,[edi+TShape.Points]
                                   dec    ebp
                                   shl    ebp,4

                                   mov    edx,ebp
                                   .IsPointsInside:
                                     mov    ecx,ebx
                                     movapd xmm0,[edi+edx-16]
                                     xorpd  xmm5,xmm5
                                     @@:movapd  xmm1,[esi+ecx-16]
                                        movapd  xmm2,[esi+ecx]
                                        movapd  xmm3,xmm1
                                        subpd   xmm1,xmm0
                                        subpd   xmm3,xmm2
                                        subpd   xmm2,xmm0
                                        shufpd  xmm3,xmm3,1
                                        xorpd   xmm2,xmm1
                                        xorpd   xmm1,xmm7
                                        movhlps xmm2,xmm2
                                        dppd    xmm1,xmm3,$33
                                        sub     ecx,16
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
                                     movapd xmm6,[esi+ecx]
                                     movapd xmm3,xmm6
                                     subpd  xmm3,[esi+ecx-16]
                                     shufpd xmm3,xmm3,1
                                     xorpd  xmm3,xmm7
                                     @@:movapd xmm0,[edi+edx]
                                        movapd xmm1,xmm0
                                        movapd xmm2,xmm0
                                        movapd xmm4,xmm6
                                        movapd xmm5,xmm6
                                        subpd  xmm0,[edi+edx-16]
                                        subpd  xmm1,[esi+ecx-16]
                                        subpd  xmm2,xmm6
                                        shufpd xmm0,xmm0,1
                                        subpd  xmm4,[edi+edx-16]
                                        xorpd  xmm0,xmm7
                                        subpd  xmm5,[edi+edx]
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
                                   mov    edi,[esp-4]
                                   mov    [edi+TShape.Parent],esi
                                 .exit:
                                 mov esi,[esp-8]
                                 cmp eax,[SortedShapes]
                               ja .child
                               mov eax,[esp-12]
                             .next:
                             sub eax,4
                             cmp eax,[SortedShapes]
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

                           mov     edx,[CurveInfo2]
                           mov     eax,[NumPaths]
                           mov     edi,[SortedShapes]
                           lea     edi,[edi+eax*4-4]
                           mov     ebp,[NumPoints]
                           shl     ebp,4
                           add     ebp,[Points]
                           push    [edx+SAFEARRAY.pvData]
                           mov     [edx+SAFEARRAY.pvData],ebp
                           ._parent:
                             mov eax,[edi]
                             mov esi,edi
                             sub edi,4
                             cmp [eax+TShape.Parent],-1
                             je .nxtParent
                               mov ecx,[eax+TShape.Points]
                               mov [eax+TShape.Parent],ecx
                               mov ebp,[edx+SAFEARRAY.pvData]
                               ._child:
                                 mov eax,[esi]
                                 sub esi,4
                                 cmp [eax+TShape.Parent],ecx
                                 jne .nxtChild
                                   mov [eax+TShape.Parent],-1
                                   mov ebx,[eax+TShape.DataLen]
                                   mov eax,[eax+TShape.Data]
                                   shl ebx,5
                                   add ebp,ebx
                                   add eax,ebx
                                   neg ebx
                                   @@:movdqu xmm0,[eax+ebx]
                                      movdqu xmm1,[eax+ebx+16]
                                      movdqa [ebp+ebx],xmm0
                                      movdqa [ebp+ebx+16],xmm1
                                      add    ebx,32
                                   jne @b
                                 .nxtChild:
                                 cmp esi,[SortedShapes]
                               jae ._child
                               sub     ebp,[edx+SAFEARRAY.pvData]
                               shr     ebp,5
                               mov     [edx+SAFEARRAY.rgsabound.cElements],ebp
                               push    edx
                               cominvk Curve2,PutCurveInfo,CurveInfo2,ebp,Curve
                               cominvk Layer,CreateCurve,[Curve2],Curve
                               cominvk Curve,Release
                               pop     edx
                             .nxtParent:
                             cmp edi,[SortedShapes]
                           jae ._parent

                           pop     [edx+SAFEARRAY.pvData]
                           mov     [edx+SAFEARRAY.rgsabound.cElements],0
                           invoke  SafeArrayDestroy,edx
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

                           popad
                         .finish:
                         pop    ebx
                         xor eax,eax
                         ret 36
  .OnUpdatePluginCommand:push   ebx
                         mov    ebx,[esp+28]
                         mov    ebx,[ebx+DISPPARAMS.rgvarg]
                         invoke lstrcmpW,dword[ebx+sizeof.VARIANT*2+VARIANT.data],strSmartDepart
                         test   eax,eax
                         jne    @f
                           mov eax,dword[ebx+sizeof.VARIANT*1+VARIANT.data]
                           mov edx,[Enabled]
                           mov [eax],dx
                         @@:
                         pop    ebx
                         xor    eax,eax
                         ret 36

OnLoad:           ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  xchg    ebx,[esp+8]
  mov     [CorelApp],ebx
  comcall ebx,IVGApplication,AddRef
  comcall ebx,IVGApplication,Get_VersionMinor,CorelVersion
  comcall ebx,IVGApplication,Get_VersionMajor,CorelVersion+1
  mov     ebx,[esp+8]
ret 8

StartSession:     ;(const self:IVGAppPlugin):LongInt;stdcall;
  push    ebx
  mov     eax,1
  cpuid
  test    ecx,1 shl 19 ;SSE 4.1
  je @f
    mov     ebx,[CorelApp]
    comcall ebx,IVGApplication,AddPluginCommand,strSmartDepart,strButtonCaption,strButtonCaption,tmp
    comcall ebx,IVGApplication,AdviseEvents,IPlugin,EventsCookie
    comcall ebx,IVGApplication,QueryInterface,IID_ICUIApplication,CUIApp
    cominvk CUIApp,Get_DataContext,DataContext
    cominvk DataContext,GetDataSource,strSelectionInfoDatasource,SelectionInfo
    cominvk DataContext,GetDataSource,strWDocCommands,WDocCommands
    cominvk DataContext,Release
    cominvk CUIApp,Release
    pop     ebx
    xor     eax,eax
    ret 4
  @@:
  invoke MessageBoxW,0,errCPUNotSupported,strSmartDepart,MB_TASKMODAL
  pop    ebx
  mov    eax,E_FAIL
ret 4

StopSession:      ;(const self:IVGAppPlugin):LongInt;stdcall;
  cominvk CorelApp,UnadviseEvents,[EventsCookie]
  cominvk SelectionInfo,Release
  cominvk WDocCommands,Release
  xor     eax,eax
ret 4

OnUnload:         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CorelApp,Release
  xor     eax,eax
ret 4

align 16
IID_ICUIApplication        db 00ah,000h,0eeh,09ch,0a0h,042h,080h,059h,043h,0a3h,07ah,0a7h,014h,061h,048h,02ch
chs                        dq 8000000000000000h,0
dbl_05                     dq 0.5,0.5
precision                  dq 0.04
IPlugin                    dd IPluginVMT
IPluginVMT                 dd QueryInterface,\
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
errCPUNotSupported         du 'Процессор не поддерживается. Требуется SSE 4.1.',0

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
CurveInfo        rd 1
CurveInfo2       rd 1
Shapes           rd 1
SortedShapes     rd 1
Points           rd 1
CorelVersion     rd 1
NumPaths         rd 1
NumPoints        rd 1
EventsCookie     rd 1
varTmp           VARIANT
Enabled          rd 1
tmp              rd 1