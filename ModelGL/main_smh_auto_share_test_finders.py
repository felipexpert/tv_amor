from utils.utils_autogui import wait_for_img, wait_for_img_from_imgs
from utils_smh.utils_paths_config import Paths


if __name__ == "__main__":
    print("Vai tentar encontrar")
    # wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL])
    # wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL])
    wait_for_img(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL)
    # wait_for_img(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST) 