    !*****************************************************************
    !************************** Tiff module **************************
    !  Nie qiyang 2022.12.13
    !  聂启阳 上海勘测设计研究院
    !  根据tiff格式标准读取网络矩阵和无数据值。暂时未支持分带存储的数据格式，如需要拓展可针对DN==273部分专项分带读取。

    module Tiff
    Use, Intrinsic :: ISO_C_BINDING
    Implicit None
    save
    character(len=2) :: IFH_PaiXu
    Integer(kind=C_SHORT) :: IFH_V,IFD_DE_N  !C_SHORT占2字节
    Integer(kind=C_SHORT) , allocatable:: DE_tag2(:)
    Integer , allocatable:: DE_tag4(:)
    Integer(kind=C_LONG) ::DiZhi
    Integer::IFH_D
    Integer::DN,hang,lie,tou,nodateN
    Real , allocatable :: dat(:,:) !// 网格矩阵
    Character(len=:) , allocatable::nodatecha,nodatechb
    !character(len=7)::nodatecha
    real::nodate

    contains !模块过程

    subroutine Tiff_read(FileName)
    integer::i
    Character(len=:) , allocatable ,intent(in):: FileName
    Open( 12 , File = FileName , access="stream" , form = "unformatted" )
    !图像文件头IFH
    Read( 12) IFH_PaiXu  !1-2字节,II（十六进制49 49）表示Intel格式 ，低字节在前，高字节在后;MM（十六进制4D 4D）表示Motorala格式，高字节在前，低字节在后。
    !write(*,*)IFH_PaiXu
    Read( 12 ) IFH_V   !3-4字节,代表版本号：42=普通版本；43=big TIFF版本。
    !write(*,*)IFH_V
    Read( 12 )IFH_D    !5-8字节,指向文件的第一个图像文件目录的所在字节位置。如果为8，则紧邻文件后。
    !write(*,*)IFH_D
    !一个TIFF文件中至少有1个IFD,一个IFD里至少有一个DE。DE的个数是不定的，因为每个DE只标识了图像的一个属性.
    Read( 12 )IFD_DE_N   !2个字节，表征本IFD中DE(Directory Entry,目录项)的数量。
    !write(*,*)IFD_DE_N

    allocate(DE_tag2(IFD_DE_N),DE_tag4(IFD_DE_N))
    do i=1,IFD_DE_N
        write(*,*)'Tag',i,':'
        Read(12)DE_tag2(i)    !2个字节，标签编号  dem一般第一个是宽
        DN=DE_tag2(i)
        write(*,*)DE_tag2(i)
        Read(12)DE_tag2(i)    !2个字节，数据类型
        write(*,*)DE_tag2(i)
        Read(12)DE_tag4(i)    !4个字节，该类型数据的数量
        write(*,*)DE_tag4(i)
        Read(12)DE_tag4(i)    !4个字节，如果占用的字节数少于4， 则数据直接存于此。 如果超过4个，则这里存放的是指向实际数据的指针。DEM一般是前者。
        if(DN==256)then
            lie=DE_tag4(i)
        elseif(DN==257)then
            hang=DE_tag4(i)
        elseif(DN==273)then
            tou=DE_tag4(i)
        elseif(DN== -23423)then  !tiff中这个属性名为无符号短整形，fortran都是有符号变量，因此为负。
            nodateN=DE_tag4(i)
        endif
        write(*,*)DE_tag4(i)
    enddo
    !write(*,*)hang,lie,tou
    Read( 12,Pos=tou+1 )Dizhi
    tou=Dizhi+1   !获取到数据的具体地址

    nodatecha='          '
    nodatechb='          '
    Read( 12,Pos=nodateN+1 )nodatechb  !根据nodata地址检索nodata值
    Read(nodatechb,*)nodatecha
    nodatecha=trim(nodatecha)
    Read(nodatecha,*)nodate
    write(*,*)nodate

    allocate(dat(lie,hang))

    Read( 12,Pos=tou )dat
    close(12)
    return
    end subroutine Tiff_read

    end module Tiff
    !************************** Tiff module **************************
    !*****************************************************************




    program name
    Use Tiff
    Use, Intrinsic :: ISO_C_BINDING
    Implicit None

    Real::k
    Character(len=:) , allocatable:: fileOld,fileNew
    !Open( 12 , File = "C:\Users\nieqi\Desktop\L\10shihou_cai.tif" , access="stream" , form = "unformatted" )

    integer::Zhuangtai,kk,i
    integer,external::SYSTEM
    fileOld="C:\Users\nieqi\Desktop\L\10shihou_cai.tif"

    fileNew=fileOld(1:len(fileOld)-4)//'_change.tif'
    write(*,*)fileNew

    kk=SYSTEM('copy '//fileOld//' '//fileNew)

    call Tiff_read(fileNew)  !调用Tiff读取的
    write(*,*)dat(1,1)

    dat=1.0
    Open( 12 , File = fileNew , access="stream" , form = "unformatted" )
    !write(*,*)dat(6,1)
    write(12,Pos=tou)dat
    close(12)
    deallocate(DE_tag2,DE_tag4,dat)
    end program name