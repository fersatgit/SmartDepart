use32
format PE GUI 4.0 DLL as 'cpg'
entry DllEntryPoint
include 'encoding\win1251.inc'
include 'win32w.inc'

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

align 16
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
ret 12

AttachPlugin: ;ppIPlugin: IVGAppPlugin
  mov eax,[esp+4]
  mov dword[eax],IPlugin
  mov eax,256
ret 4

;De Casteljau's curve linearization
;xmm0-xmm3 - bezier coordinates (double precision)
;xmm7 - double with 1 in exponential part and 0 in others
;precision - square of a maximum deviation in tenths micron
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
  comisd  xmm3,[precision]
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
    movapd  [edi],xmm0
    add     edi,16
    call    Bezier2Polyline
  @@:

  add     esp,40h
retn

;;;;;;;;;;;;;;;ITypeComp;;;;;;;;;;;;;;;;;;;;;;
Bind: ;(self: ITypeComp; const szName: WideString; lHashVal: Longint; wflags: Word; out tinfo: ITypeInfo; out desckind: Longint; out bindptr: pointer): HResult; stdcall;
  mov   eax,[esp+8]
  movzx eax,byte[eax]
  sub   eax,'0'
  mov   [funcdesc.memid],eax
  mov   eax,[esp+24]
  mov   dword[eax],DESCKIND_FUNCDESC
  mov   eax,[esp+28]
  mov   dword[eax],funcdesc
  movzx edx,word[esp+16]
  xor   eax,eax
  mov   [funcdesc.invkind],edx
  cmp   edx,INVOKE_PROPERTYGET
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_R8
    ret 28
  @@:
  cmp   edx,INVOKE_PROPERTYPUT
  jne @f
    mov [funcdesc.cParams],1
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret 28
  @@:
  cmp   edx,INVOKE_FUNC
  jne @f
    mov [funcdesc.cParams],0
    mov [funcdesc.elemdescFunc.tdesc.vt],VT_VOID
    ret 28
  @@:
ret 28

;;;;;;;;;;;;;;;ITypeInfo;;;;;;;;;;;;;;;;;;;;;;
ReleaseTypeAttr:      ;(self: ITypeInfo; ptypeattr: PTypeAttr); stdcall;
ReleaseFuncDesc:      ;(self: ITypeInfo; pfuncdesc: pointer); stdcall;
ReleaseVarDesc:       ;(self: ITypeInfo; pvardesc: PVarDesc); stdcall;
GetTypeAttr:          ;(self: ITypeInfo; out ptypeattr: PTypeAttr): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 8

GetTypeComp:          ;(self: ITypeInfo; out tcomp: ITypeComp): HResult; stdcall;
  mov eax,[esp+8]
  mov dword[eax],ITypeComp
  xor eax,eax
ret 8

GetFuncDesc:             ;(self: ITypeInfo; index: Integer; out pfuncdesc: pointer): HResult; stdcall;
GetVarDesc:              ;(self: ITypeInfo; index: Integer; out pvardesc: PVarDesc): HResult; stdcall;
GetRefTypeOfImplType:    ;(self: ITypeInfo; index: Integer; out reftype: dword): HResult; stdcall;
GetImplTypeFlags:        ;(self: ITypeInfo; index: Integer; out impltypeflags: Integer): HResult; stdcall;
GetRefTypeInfo:          ;(self: ITypeInfo; reftype: dword; out tinfo: IUnknown): HResult;stdcall;
GetMops:                 ;(self: ITypeInfo; memid: Longint; out bstrMops: WideString): HResult;stdcall;
GetContainingTypeLib:    ;(self: ITypeInfo; out tlib: IUnknown; out pindex: Integer): HResult;stdcall;
  mov eax,E_NOTIMPL
ret 12

GetNames:                ;(self: ITypeInfo; memid: Longint; rgbstrNames: pointer; cMaxNames: Integer; out cNames: Integer): HResult; stdcall;
BindType:                ;(self: ITypeComp; const szName: WideString; lHashVal: Longint; out tinfo: ITypeInfo; out tcomp: ITypeComp): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 20

ITypeInfo.GetIDsOfNames: ;(self: ITypeInfo; rgpszNames: pointer; cNames: Integer; rgmemid: pointer): HResult; stdcall;
AddressOfMember:         ;(self: ITypeInfo; memid: Longint; invkind: Longint; out ppv: Pointer): HResult; stdcall;
CreateInstance:          ;(self: ITypeInfo; const unkOuter: IUnknown; const iid: TGUID; out vObj): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 16

ITypeInfo.Invoke:        ;(self: ITypeInfo; pvInstance: Pointer; memid: Longint; flags: Word; var dispParams: DispParams; varResult: PVariant; excepInfo: pointer; argErr: PInteger): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 32

GetDocumentation:        ;(self: ITypeInfo; memid: Longint; pbstrName: PWideString; pbstrDocString: PWideString; pdwHelpContext: PLongint; pbstrHelpFile: PWideString): HResult; stdcall;
GetDllEntry:             ;(self: ITypeInfo; memid: Longint; invkind: Longint; bstrDllName, bstrName: PWideString; wOrdinal: PWord): HResult;stdcall;
  mov eax,E_NOTIMPL
ret 24


;;;;;;;;;;;;;;;IVGPlugin;;;;;;;;;;;;;;;;;;;;;;
QueryInterface:   ;(const self:IVGAppPlugin; const IID: TGUID; out Obj): HResult; stdcall;
  mov eax,[esp+12]
  mov dword[eax],IPlugin
  xor eax,eax
ret 12
StopSession:      ;(const self:IVGAppPlugin):Integer; stdcall;
AddRef:           ;(const self:IVGAppPlugin):Integer; stdcall;
Release:          ;(const self:IVGAppPlugin):Integer; stdcall;
  xor eax,eax
ret 4
GetTypeInfoCount: ;(const self:IVGAppPlugin; out Count: Integer): HResult; stdcall;
  mov eax,[esp+8]
  mov dword[eax],1
  xor eax,eax
ret 8
GetTypeInfo:      ;(const self:IVGAppPlugin; Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
  mov eax,[esp+16]
  mov dword[eax],ITypeInfo
  xor eax,eax
ret 16
GetIDsOfNames:    ;(const self:IVGAppPlugin; const IID: TGUID; Names: Pointer;NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
  mov eax,E_NOTIMPL
ret 24


Invoke:           ;(const self:IVGAppPlugin; DispID: Integer; const IID: TGUID; LocaleID: Integer;Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  mov eax,[esp+8]
  cmp eax,4
  jne .OnInvoke
    test word[esp+20],INVOKE_PROPERTYGET ;Flags
    je @f
      mov   eax,[esp+28]
      movsd xmm0,[MaxDeviation]
      mov   dword[eax+VARIANT.type],VT_R8
      movsd [eax+VARIANT.data],xmm0
      xor   eax,eax
      ret 36
    @@:
    test word[esp+20],INVOKE_PROPERTYPUT ;Flags
    je @f
      mov   eax,[esp+24]
      mov   eax,[eax+DISPPARAMS.rgvarg]
      movsd xmm0,[eax+VARIANT.data]
      movsd [MaxDeviation],xmm0
    @@:
    xor eax,eax
    ret 36
  .OnInvoke:
  ja .finish
    pushad
    mov     [cmd],eax
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
    cominvk SubPaths,Release
    invoke  GlobalMemoryStatusEx,memstatus
    invoke  GetSystemInfo,sysInfo
    xor     eax,eax
    cmp     eax,dword[memstatus.ullAvailVirtual+4]
    sbb     eax,eax
    or      dword[memstatus.ullAvailVirtual],eax
    @@:shr     dword[memstatus.ullAvailVirtual],1                                         ;Allocating maximum memory area, because we don`t now how many verticles we will get after bezier linearization
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Converting curve data from CorelDraw to list of a linear segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov     eax,[CurveInfo]
    mov     esi,[eax+SAFEARRAY.pvData]
    mov     eax,[eax+SAFEARRAY.rgsabound.cElements]
    shl     eax,5
    add     eax,esi
    mov     ebp,[Shapes]
    mov     edi,[Points]
    mov     [NumPoints],0
    movsd   xmm0,[MaxDeviation]
    mulsd   xmm0,[dbl_10000]
    mulsd   xmm0,xmm0
    movsd   [precision],xmm0
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
                          movapd   xmm7,dqword[dbl_exp_1]
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
    invoke  VirtualFree,eax,dword[memstatus.ullAvailVirtual],MEM_DECOMMIT                         ;Free unneeded memory part

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculating bounding box and it square for each Shape (It faster than calling GetBoundingBox method)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sorting shapes by bounding box square
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Determining parent relations between shapes (smart dividing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      jne .next                              ;Open curves can`t be a parent
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
              je     .exit                   ;if any point lie outside - this is not a child
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
                 jc     .exit                ;if any edges intersect - thiis is not a child
                 sub    edx,16
              jne @b
              sub ecx,16
            jne .IsEdgesIntersect
            mov    edi,[esp-4]
            mov    [edi+TShape.Parent],esi   ;all checks passed - filling the "parent" field
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
    mov     edx,[CurveInfo]
    push    [edx+SAFEARRAY.rgsabound.cElements]
    push    [edx+SAFEARRAY.pvData]
    cmp     [cmd],0
    jne .Malloc
      mov     eax,[NumPaths]
      mov     edi,[SortedShapes]
      lea     edi,[edi+eax*4-4]
      mov     ebp,[NumPoints]
      shl     ebp,4
      add     ebp,[Points]
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
          cominvk Curve2,PutCurveInfo,CurveInfo,ebp,Curve
          cominvk Layer,CreateCurve,[Curve2],Curve
          cominvk Curve,Release
          pop     edx
        .nxtParent:
        cmp edi,[SortedShapes]
      jae ._parent
      jmp .quit
    .Malloc:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Memory allocation for triangulation and decomposition routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov    esi,[NumPaths]
    mov    ebx,[NumPoints]
    add    ebx,esi
    sub    ebx,4
    shl    ebx,2
    mov    eax,ebx
    shl    eax,6
    lea    eax,[eax+esi*4]
    invoke VirtualAlloc,0,eax,MEM_COMMIT,PAGE_READWRITE
    shl    ebx,4
    mov    [membuf],eax
    mov    [OuterHull],eax
    add    eax,ebx
    mov    [Hull],eax
    add    eax,ebx
    mov    [CurveElements],eax
    lea    eax,[eax+ebx*2]
    mov    [Holes],eax
    mov    [CurveElementsLen],0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ensure that paths is clockwise oriented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    movapd xmm6,dqword[chs]
    xorpd  xmm7,xmm7
    mov    ebp,[SortedShapes]
    .CheckClockwise:
        mov      edi,[ebp+esi*4-4]
        mov      edx,[edi+TShape.Points]
        mov      ecx,[edi+TShape.NumPoints]
        shl      ecx,4
        movapd   xmm0,[edx]
        cmpeqpd  xmm0,[edx+ecx-16]
        movmskpd eax,xmm0
        cmp      eax,3                     ;If curve is open - mark it to skip
        je @f
          mov [edi+TShape.Parent],-1
          jmp .Clockwise
        @@:
        movsd  xmm0,[dbl_INF]
        lea    eax,[ecx-16]
        @@:comisd xmm0,[edx+eax-16]        ;Find most left vertex
            jbe .f1
              movsd xmm0,[edx+eax-16]
              mov   ebx,eax
            .f1:
            sub    eax,16
        jne @b
        movapd xmm0,[edx+ebx-16]           ;check direction (cross-product between edges)
        cmp    ebx,16
        mov    eax,ebx
        cmove  eax,ecx
        sub    eax,32
        movapd xmm1,[edx+eax]
        movapd xmm2,[edx+ebx]
        subpd  xmm0,xmm1
        subpd  xmm2,xmm1
        shufpd xmm0,xmm0,1
        xorpd  xmm0,xmm6
        dppd   xmm0,xmm2,110011b
        comisd xmm7,xmm0
        jae .Clockwise
          lea    eax,[ecx-16]
          shr    eax,5
          shr    ecx,5
          shl    eax,4
          shl    ecx,4
          @@:movapd xmm0,[edx+eax]         ;if not clockwise - reverse order
             movapd xmm1,[edx+ecx]
             movapd [edx+ecx],xmm0
             movapd [edx+eax],xmm1
             add    ecx,16
             sub    eax,16
          jne @b
        .Clockwise:
        dec esi
    jne .CheckClockwise

    .nxtPath:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Slicing polygon with holes to a simple non-convex polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      mov ecx,[NumPaths]
      dec ecx
      mov ebx,[SortedShapes]
      mov esi,[ebx+ecx*4]
      cmp [esi+TShape.Parent],-1
      je .skipPath
        mov eax,[esi+TShape.NumPoints]
        mov esi,[esi+TShape.Points]
        mov [OuterHullLen],eax
        mov ebp,[OuterHull]
        shl eax,4
        @@:movapd xmm0,[esi+eax-16]
           movapd [ebp+eax-16],xmm0
           sub    eax,16
        jne @b

        test ecx,ecx
        je .NoHoles
        mov edi,[Holes]
        mov edx,ecx
        .MakeHolesList:
            mov eax,[ebx+edx*4-4]
            cmp [eax+TShape.Parent],esi
            jne @f
              mov [edi],eax
              add edi,4
              mov [eax+TShape.Parent],-1
            @@:
            dec edx
        jne .MakeHolesList

        sub edi,[Holes]
        shr edi,2
        je .NoHoles
          mov   [HolesCount],edi
          .Slice:
          ;Searching pair of a closest edges in outer hull and hole
            movsd xmm0,[dbl_INF]
            movsd [minDist],xmm0
            mov   edx,[Holes]
            mov   ebx,[OuterHullLen]
            sub   ebx,2
            shl   ebx,4
            .FindClosestEdges:
              mov ecx,[HolesCount]
              .NextHole:
                mov esi,[edx+ecx*4-4]
                mov edi,[esi+TShape.NumPoints]
                mov esi,[esi+TShape.Points]
                sub edi,2
                shl edi,4
                .NextEdge:
                  movapd xmm4,[esi+edi]    ;Calculating distance between edges AB and CD
                  movapd xmm5,[esi+edi+16]
                  movapd xmm6,[ebp+ebx]
                  movapd xmm7,[ebp+ebx+16]
                  movapd xmm0,xmm5
                  movapd xmm1,xmm7
                  subpd  xmm0,xmm4         ;AB
                  subpd  xmm1,xmm6         ;CD
                  subpd  xmm7,xmm4         ;AD
                  subpd  xmm5,xmm6         ;CB
                  subpd  xmm4,xmm6         ;CA
                  subpd  xmm6,[esi+edi]    ;AC
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
                  movapd xmm2,[esi+edi]    ;A
                  movapd xmm3,[ebp+ebx]    ;C
                  mulpd  xmm4,xmm1
                  mulpd  xmm5,xmm1
                  mulpd  xmm6,xmm0
                  mulpd  xmm7,xmm0
                  addpd  xmm4,xmm3         ;pa - point A projection on the edge CD (clamped to CD)
                  addpd  xmm5,xmm3         ;pb - point B projection on the edge CD (clamped to CD)
                  addpd  xmm6,xmm2         ;pc - point C projection on the edge AB (clamped to AB)
                  addpd  xmm7,xmm2         ;pd - point D projection on the edge AB (clamped to AB)
                  subpd  xmm4,xmm2
                  subpd  xmm5,[esi+edi+16]
                  subpd  xmm6,xmm3
                  subpd  xmm7,[ebp+ebx+16]
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
                    mov   [esp-4],ecx  ;ClosestHole
                    mov   [esp-8],ebx  ;ClosestEdge
                    mov   [esp-12],edi ;ClosestHoleEdge
                  @@:
                  sub    edi,16
                jns .NextEdge
                dec ecx
              jne .NextHole
              sub ebx,16
            jns .FindClosestEdges

          ;Remove closest hole from list of holes
            mov ecx,[esp-4]     ;ClosestHole
            mov esi,[edx+ecx*4-4]
            mov  ebx,[HolesCount]
            lea  edi,[edx+ecx*4]
            sub  ebx,ecx
            lea  edi,[edi+ebx*4]
            je .f5
              neg ebx
              @@:mov eax,[edi+ebx*4]
                  mov [edi+ebx*4-4],eax
                  inc ebx
              jne @b
            .f5:

          ;Choose pair of a closest vertexes in closest edges
            mov ebx,[esp-8] ;ClosestEdge
            mov edi,[esp-12];ClosestHoleEdge
            mov ecx,[esi+TShape.Points]
            movapd xmm4,[ebp+ebx]
            movapd xmm5,[ebp+ebx+16]
            movapd xmm0,[ecx+edi]
            movapd xmm1,[ecx+edi+16]
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
            mov    eax,ebx
            mov    edx,edi
            comisd xmm0,xmm1
            jna @f
              movapd xmm0,xmm1
              lea    edx,[edi+16]
            @@:
            comisd xmm0,xmm2
            jna @f
              movapd xmm0,xmm2
              lea    eax,[ebx+16]
              mov    edx,edi
            @@:
            comisd xmm0,xmm3
            jna @f
              lea    eax,[ebx+16]
              lea    edx,[edi+16]
            @@:

          ;Build new outer hull from current outer hull and closest hole
            mov ebx,eax
            mov edi,[Hull]            ;ebp - OuterHull
            @@:movapd xmm0,[ebp+ebx]
               movapd [edi+ebx],xmm0
               sub    ebx,16
            jns @b
            lea edi,[edi+eax+16]
            mov ebx,edx               ;ecx - Holes[ClosestHole].Points
            @@:movapd xmm0,[ecx+ebx]
               movapd [edi],xmm0
               add    edi,16
               sub    ebx,16
            jns @b
            mov ebx,[esi+TShape.NumPoints]
            sub ebx,2
            shl ebx,4
            add ecx,edx
            sub ebx,edx
            js .nxt
              @@:movapd xmm0,[ecx+ebx]
                 movapd [edi],xmm0
                 add    edi,16
                 sub    ebx,16
              jns @b
            .nxt:
            mov ebx,[OuterHullLen]
            shl ebx,4
            sub ebx,eax
            add ebp,eax
            lea edx,[edi+ebx]
            @@:movapd xmm0,[ebp+ebx-16]
               movapd [edi+ebx-16],xmm0
               sub    ebx,16
            jne @b

            sub  edx,[Hull]
            shr  edx,4
            mov  [OuterHullLen],edx
            mov  ebp,[OuterHull]
            xchg ebp,[Hull]
            mov  [OuterHull],ebp

            dec [HolesCount]
          jne .Slice
        .NoHoles:

        cmp [cmd],1   ;if cmd is "String slice"
        jne .NoSlice
          mov edx,[OuterHullLen]
          shl edx,4
          mov edi,[CurveElementsLen]
          lea ecx,[edx+edx]
          add edi,[CurveElements]
          add [CurveElementsLen],ecx
          @@:movapd xmm0,[ebp+edx-16]
             movapd dqword[edi+edx*2-32+CurveElement.PositionX],xmm0
             mov    [edi+edx*2-32+CurveElement.ElementType],cdrElementLine
             mov    [edi+edx*2-32+CurveElement.NodeType],cdrCuspNode
             mov    [edi+edx*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             sub    edx,16
          jne @b
          mov    [edi+CurveElement.ElementType],cdrElementStart
          mov    [edi+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          mov    [edi+ecx-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
          jmp .skipPath
      .NoSlice:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ear-clip triangulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        mov ebp,[Hull]
        mov eax,[OuterHull]
        mov esi,[OuterHullLen]
        xor ebx,ebx
        sub esi,2
        shl esi,4
        jmp .EarClipStart
        .EarClip:
          lea edi,[esi-16]
          .FindEar:
            movapd xmm0,[eax+edi]
            movapd xmm1,[eax+edi+16]
            movapd xmm2,[eax+edi+32]
            movapd xmm3,xmm1
            movapd xmm4,xmm2
            subpd  xmm3,xmm0
            subpd  xmm4,xmm0
            movapd xmm6,dqword[chs]
            shufpd xmm3,xmm3,1
            xorpd  xmm3,xmm6
            dppd   xmm4,xmm3,110011b
            xorpd  xmm5,xmm5
            comisd xmm4,xmm5
            jae .Continue ;if angle between edges is concave - skip
              movapd xmm4,xmm2
              movapd xmm5,xmm0
              subpd  xmm4,xmm1
              subpd  xmm5,xmm2
              shufpd xmm4,xmm4,1
              shufpd xmm5,xmm5,1
              xorpd  xmm4,xmm6
              xorpd  xmm5,xmm6
              mov    edx,esi
              .NextVertex:
                movapd   xmm6,[eax+edx]
                subpd    xmm6,xmm0
                dppd     xmm6,xmm3,110011b
                cvtsd2ss xmm7,xmm6
                pslldq   xmm7,4
                movapd   xmm6,[eax+edx]
                subpd    xmm6,xmm1
                dppd     xmm6,xmm4,110011b
                cvtsd2ss xmm7,xmm6
                pslldq   xmm7,4
                movapd   xmm6,[eax+edx]
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
              movapd [ebp+ebx],xmm0
              movapd [ebp+ebx+16],xmm1
              movapd [ebp+ebx+32],xmm2
              movapd [ebp+ebx+48],xmm0
              add    ebx,64

              ;Remove ear vertex from polygon
              mov    ecx,esi
              sub    ecx,edi
              je .skip
                add    edi,ecx
                add    edi,eax
                neg    ecx
                @@:movapd xmm0,[edi+ecx+32]
                   movapd [edi+ecx+16],xmm0
                   add    ecx,16
                jne @b
              .skip:
              sub    esi,16
              jmp .EarClipStart
            .Continue:
            sub edi,16
          jns .FindEar
          .EarClipStart:
          cmp esi,16
        ja .EarClip
        mov [HullLen],ebx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Merging to convex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        mov    [ESP_Preserve],esp
        mov    [OuterHullLen],0
        movapd xmm6,dqword[chs]
        cmp    [cmd],2
        jne    .Merging     ;for triangulation just generate PutCurveInfo data
          mov edi,[CurveElementsLen]
          add edi,[CurveElements]
          mov esi,[Hull]
          mov ebp,ebx
          lea eax,[ebp*2]
          @@:movapd xmm0,[esi+ebp-64]
             movapd xmm1,[esi+ebp-48]
             movapd xmm2,[esi+ebp-32]
             movapd xmm3,[esi+ebp-16]
             movapd dqword[edi+ebp*2-128+CurveElement.PositionX],xmm0
             mov    [edi+ebp*2-128+CurveElement.ElementType],cdrElementStart
             mov    [edi+ebp*2-128+CurveElement.NodeType],cdrCuspNode
             mov    [edi+ebp*2-128+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
             movapd dqword[edi+ebp*2-96+CurveElement.PositionX],xmm1
             mov    [edi+ebp*2-96+CurveElement.ElementType],cdrElementLine
             mov    [edi+ebp*2-96+CurveElement.NodeType],cdrCuspNode
             mov    [edi+ebp*2-96+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             movapd dqword[edi+ebp*2-64+CurveElement.PositionX],xmm2
             mov    [edi+ebp*2-64+CurveElement.ElementType],cdrElementLine
             mov    [edi+ebp*2-64+CurveElement.NodeType],cdrCuspNode
             mov    [edi+ebp*2-64+CurveElement.Flags],cdrFlagValid+cdrFlagUser
             movapd dqword[edi+ebp*2-32+CurveElement.PositionX],xmm3
             mov    [edi+ebp*2-32+CurveElement.ElementType],cdrElementLine
             mov    [edi+ebp*2-32+CurveElement.NodeType],cdrCuspNode
             mov    [edi+ebp*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
             sub    ebp,64
          jne @b
        jmp .quit2
          .Merging:
            sub    [HullLen],64
            mov    esp,64
            mov    eax,[OuterHullLen]
            mov    edi,[OuterHull]
            add    edi,eax
            mov    esi,[Hull]
            mov    ebp,[HullLen]
            movapd xmm0,[esi+ebp]
            movapd xmm1,[esi+ebp+16]
            movapd xmm2,[esi+ebp+32]
            movapd xmm3,[esi+ebp+48]
            movapd [edi],xmm0
            movapd [edi+16],xmm1
            movapd [edi+32],xmm2
            movapd [edi+48],xmm3

            .Merge:
            mov ebp,[HullLen]
            jmp .ScanTrianglesStart
            .ScanTriangles:
              mov ebx,8
              .ScanTriEdges:
                lea eax,[esp-16]
                .ScanPolyEdges:
                  lea      ecx,[esi+ebp]
                  movapd   xmm0,[edi+eax]
                  movapd   xmm1,[edi+eax-16]
                  cmpeqpd  xmm0,[ecx+ebx*4]
                  cmpeqpd  xmm1,[ecx+ebx*4+16]
                  pand     xmm0,xmm1
                  movmskpd ecx,xmm0
                  cmp      ecx,3
                  jne .Continue2                    ;if polygon edge is equal to triangle edge...
                    cmp     eax,16
                    mov     ecx,eax
                    cmove   ecx,esp
                    sub     ecx,32
                    movapd  xmm0,[edi+ecx]
                    lea     ecx,[eax+16]
                    mov     edx,16
                    cmp     ecx,esp
                    cmove   ecx,edx
                    movapd  xmm1,[edi+eax-16]
                    movapd  xmm4,[edi+eax]
                    movapd  xmm5,[edi+ecx]
                    mov     edx,dword[TranslateOffset+ebx]
                    add     edx,ebp
                    movapd  xmm2,[esi+edx]
                    movapd  xmm3,xmm2
                    subpd   xmm1,xmm0
                    subpd   xmm2,xmm0
                    subpd   xmm4,xmm3
                    subpd   xmm5,xmm3
                    shufpd  xmm1,xmm1,1
                    shufpd  xmm4,xmm4,1
                    xorpd   xmm1,xmm6
                    xorpd   xmm4,xmm6
                    dppd    xmm1,xmm2,110011b
                    dppd    xmm4,xmm5,110011b
                    xorpd   xmm0,xmm0
                    cmplepd xmm1,xmm0
                    cmplepd xmm4,xmm0
                    ptest   xmm1,xmm4
                    je .Continue2                   ;and if polygon merging with triangle is convex shape
                      lea edx,[edi+eax]             ;then merge polygon with triangle
                      mov ecx,esp
                      sub ecx,eax
                      @@:movapd xmm0,[edx+ecx-16]   ;add triangle vertex to polygon
                         movapd [edx+ecx],xmm0
                         sub    ecx,16
                      jne @b
                      movapd [edx],xmm3
                      add    esp,16
                      mov    ecx,ebp
                      mov    edx,[HullLen]
                      sub    ecx,edx
                      add    ecx,64
                      je .skip2
                        add    edx,esi
                        @@:movapd xmm0,[edx+ecx]    ;remove triangle from list
                           movapd [edx+ecx-64],xmm0
                           add    ecx,16
                        jne @b
                      .skip2:
                      sub [HullLen],64
                      jmp .Merge
                  .Continue2:
                  sub eax,16
                jne .ScanPolyEdges
                sub ebx,4
              jns .ScanTriEdges
              .ScanTrianglesStart:
              sub ebp,64
            jns .ScanTriangles
            ;Generate data for PutCurveInfo
            mov esi,[OuterHullLen]
            add esi,esi
            add esi,[CurveElements]
            add esi,[CurveElementsLen]
            mov ecx,esp
            @@:movapd xmm0,[edi+ecx-16]
               movapd dqword[esi+ecx*2-32+CurveElement.PositionX],xmm0
               mov    [esi+ecx*2-32+CurveElement.ElementType],cdrElementLine
               mov    [esi+ecx*2-32+CurveElement.NodeType],cdrCuspNode
               mov    [esi+ecx*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser
               sub    ecx,16
            jne @b
            mov ecx,esp
            mov [esi+CurveElement.ElementType],cdrElementStart
            mov [esi+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
            mov [esi+ecx*2-32+CurveElement.Flags],cdrFlagValid+cdrFlagUser+cdrFlagClosed
            add [OuterHullLen],esp
            cmp [HullLen],0
          jne .Merging
          mov eax,[OuterHullLen]
          shr [OuterHullLen],4
          add eax,eax
        .quit2:
        add [CurveElementsLen],eax
        mov esp,[ESP_Preserve]

      .skipPath:
      dec [NumPaths]
    jne .nxtPath

    mov     edx,[CurveInfo]
    mov     esi,[CurveElements]
    mov     eax,[CurveElementsLen]
    shr     eax,5
    je      .NoData
      mov     [edx+SAFEARRAY.pvData],esi
      mov     [edx+SAFEARRAY.rgsabound.cElements],eax
      cominvk Curve2,PutCurveInfo,CurveInfo,eax,Curve
      cominvk Layer,CreateCurve,[Curve2],Curve
      cominvk Curve,Release
    .NoData:
    invoke  VirtualFree,[membuf],0,MEM_RELEASE
    .quit:
    mov     edx,[CurveInfo]
    pop     [edx+SAFEARRAY.pvData]
    pop     [edx+SAFEARRAY.rgsabound.cElements]
    invoke  SafeArrayDestroy,edx
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
    popad
  .finish:
  xor eax,eax
ret 36

OnLoad:           ;(const self:IVGAppPlugin; const _Application: IVGApplication):LongInt;stdcall;
  xchg    ebx,[esp+8]
  mov     [CorelApp],ebx
  comcall ebx,IVGApplication,AddRef
  comcall ebx,IVGApplication,Get_VersionMinor,CorelVersion
  comcall ebx,IVGApplication,Get_VersionMajor,CorelVersion+1
  comcall ebx,IVGApplication,QueryInterface,IID_ICUIApplication,CUIApp
  cominvk CUIApp,RegisterDataSource,strSmartDepart,DataSourceFactory,NullStr,0,minDist  ;I`ve tried CUIApp.DataContext.AddDataSource, but it cause exception at the CorelDraw closing (look`s like Corel try to release interface after dll is unloaded)
  mov     ebx,[esp+8]
ret 8

StartSession:     ;(const self:IVGAppPlugin):LongInt;stdcall;
  mov   eax,1
  cpuid
  xor   eax,eax
  and   ecx,10001b shl 19 ;SSE 4.1 and popcnt
  cmp   ecx,10001b shl 19
  je @f
    invoke  MessageBoxW,0,errCPUNotSupported,strSmartDepart,MB_TASKMODAL
    stdcall OnUnload,eax
    mov     eax,E_FAIL
  @@:
ret 4

OnUnload:         ;(const self:IVGAppPlugin)LongInt;stdcall;
  cominvk CUIApp,UnregisterDataSource,strSmartDepart,minDist
  cominvk CUIApp,Release
  cominvk CorelApp,Release
  xor     eax,eax
ret 4

;;;;;;;;;;;;;;;ICUIDataSourceFactory;;;;;;;;;;;;;;;;;;;;;;
CreateDataSource: ;((const self:ICUIDataSourceFactory;const DataSourceName: WideString; const Proxy: ICUIDataSourceProxy; out ppVal: IDispatch); safecall;
  mov edx,[esp+16]
  mov dword[edx],IPlugin
  xor eax,eax
ret 16

align 16
chs                        dq 8000000000000000h,0
dbl_exp_1                  dq 1 shl 52,1 shl 52
MaxDeviation               dq 0.1
dbl_10000                  dq 10000.0
dbl_INF                    dq 0x7ff0000000000000
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
DataSourceFactory          dd DataSourceFactoryVMT
DataSourceFactoryVMT       dd QueryInterface,\
                              AddRef,\
                              Release,\
                              GetTypeInfoCount,\
                              GetTypeInfo,\
                              GetIDsOfNames,\
                              Invoke,\
                              CreateDataSource
ITypeInfo                  dd ITypeInfoVMT
ITypeInfoVMT               dd QueryInterface,\
                              AddRef,\
                              Release,\
                              GetTypeAttr,\
                              GetTypeComp,\
                              GetFuncDesc,\
                              GetVarDesc,\
                              GetNames,\
                              GetRefTypeOfImplType,\
                              GetImplTypeFlags,\
                              ITypeInfo.GetIDsOfNames,\
                              ITypeInfo.Invoke,\
                              GetDocumentation,\
                              GetDllEntry,\
                              GetRefTypeInfo,\
                              AddressOfMember,\
                              CreateInstance,\
                              GetMops,\
                              GetContainingTypeLib,\
                              ReleaseTypeAttr,\
                              ReleaseFuncDesc,\
                              ReleaseVarDesc
ITypeComp                  dd ITypeCompVMT
ITypeCompVMT               dd QueryInterface,\
                              AddRef,\
                              Release,\
                              Bind,\
                              BindType

TranslateOffset            dd 32,0,16,32,48,16
strGraphic                 OLEstr '~graphic~'
strSmartDepart             OLEstr 'SmartDepart'
errCPUNotSupported         du 'Процессор не поддерживается. Требуется SSE 4.1 и popcnt.',0
params                     ELEMDESC 0,VT_R8
funcdesc                   FUNCDESC 0,0,params,FUNC_DISPATCH,0,CC_STDCALL

align 16
precision        rq 1
minDist          rq 1
memstatus        MEMORYSTATUSEX sizeof.MEMORYSTATUSEX
sysInfo          SYSTEM_INFO
CorelApp         IVGApplication
CUIApp           ICUIApplication
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
membuf           rd 1
Hull             rd 1
OuterHull        rd 1
CurveElements    rd 1
Holes            rd 1
CurveInfo        rd 1
Shapes           rd 1
SortedShapes     rd 1
Points           rd 1
ESP_Preserve     rd 1
HullLen          rd 1
OuterHullLen     rd 1
HolesCount       rd 1
CurveElementsLen rd 1
cmd              rd 1
CorelVersion     rd 1
NumPaths         rd 1
NumPoints        rd 1
NullStr          rq 1
































