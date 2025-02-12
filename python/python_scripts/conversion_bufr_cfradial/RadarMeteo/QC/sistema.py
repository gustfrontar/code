# this function adapted from the Scipy Cookbook:
# http://www.scipy.org/Cookbook/SignalSmooth
import numpy as np

# this function adapted from the Scipy Cookbook:
# http://www.scipy.org/Cookbook/SignalSmooth
def smooth_and_trim(x, window_len=11, window='hanning'):
    """
    Smooth data using a window with requested size.
    This method is based on the convolution of a scaled window with the signal.
    The signal is prepared by introducing reflected copies of the signal
    (with the window size) in both ends so that transient parts are minimized
    in the begining and end part of the output signal.
    Parameters
    ----------
    x : array
        The input signal
    window_len: int
        The dimension of the smoothing window; should be an odd integer.
    window : str
        The type of window from 'flat', 'hanning', 'hamming', 'bartlett',
        'blackman' or 'sg_smooth'. A flat window will produce a moving
        average smoothing.
    Returns
    -------
    y : array
        The smoothed signal with length equal to the input signal.
    """
    if x.ndim != 1:
        raise ValueError("smooth only accepts 1 dimension arrays.")
    if x.size < window_len:
        raise ValueError("Input vector needs to be bigger than window size.")
    if window_len < 3:
        return x
    valid_windows = ['flat', 'hanning', 'hamming', 'bartlett', 'blackman',
                     'sg_smooth']
    if not window in valid_windows:
        raise ValueError("Window is on of " + ' '.join(valid_windows))

    s = np.r_[x[window_len - 1:0:-1], x, x[-1:-window_len:-1]]

    if window == 'flat':  # moving average
        w = np.ones(int(window_len), 'd')
    elif window == 'sg_smooth':
        w = np.array([0.1, .25, .3, .25, .1])
    else:
        w = eval('np.' + window + '(window_len)')

    y = np.convolve(w / w.sum(), s, mode='valid')

    return y[int(window_len / 2):len(x) + int(window_len / 2)]



def det_sys_phase(radar, ncp_lev=0.4, rhohv_lev=0.6,
                  ncp_field=None, rhv_field=None, phidp_field=None):
    """
    Determine the system phase.
    Parameters
    ----------
    radar : Radar
        Radar object for which to determine the system phase.
    ncp_lev :
        Miminum normal coherent power level.  Regions below this value will
        not be included in the phase calculation.
    rhohv_lev :
        Miminum copolar coefficient level.  Regions below this value will not
        be included in the phase calculation.
    ncp_field, rhv_field, phidp_field : str
        Field names within the radar object which represent the normal
        coherent power, the copolar coefficient, and the differential phase
        shift.  A value of None for any of these parameters will use the
        default field name as defined in the Py-ART configuration file.
    Returns
    -------
    sys_phase : float or None
        Estimate of the system phase.  None is not estimate can be made.
    """
    # parse the field parameters
    if ncp_field is None:
        ncp_field = get_field_name('normalized_coherent_power')
    if rhv_field is None:
        rhv_field = get_field_name('cross_correlation_ratio')
    if phidp_field is None:
        phidp_field = get_field_name('differential_phase')

    ncp = radar.fields[ncp_field]['data'][:, 30:]
    rhv = radar.fields[rhv_field]['data'][:, 30:]
    phidp = radar.fields[phidp_field]['data'][:, 30:]
    last_ray_idx = radar.sweep_end_ray_index['data'][0]
    return _det_sys_phase(ncp, rhv, phidp, last_ray_idx, ncp_lev,rhohv_lev)

def _det_sys_phase(ncp, rhv, phidp, last_ray_idx, ncp_lev=0.4,rhv_lev=0.6):
    """ Determine the system phase, see :py:func:`det_sys_phase`. """
    good = False
    phases = []
    for radial in range(last_ray_idx + 1):
        meteo = np.logical_and(ncp[radial, :] > ncp_lev,
                               rhv[radial, :] > rhv_lev)
        mpts = np.where(meteo)
        if len(mpts[0]) > 25:
            good = True
            msmth_phidp = smooth_and_trim(phidp[radial, mpts[0]], 9)
            phases.append(msmth_phidp[0:25].min())
    if not(good):
        return None
    return np.median(phases)

